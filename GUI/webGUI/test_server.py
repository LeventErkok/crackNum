#!/usr/bin/env python3

import socket
import threading
import time
import unittest

import server


class ConcurrencyTests(unittest.TestCase):
    def setUp(self):
        self.saved_slots = server.CRACK_SLOTS
        self.saved_runner = server._run_cracknum
        self.saved_queue_secs = server.CRACK_QUEUE_SECS
        server.CRACK_QUEUE_SECS = 0.01

    def tearDown(self):
        server.CRACK_SLOTS = self.saved_slots
        server._run_cracknum = self.saved_runner
        server.CRACK_QUEUE_SECS = self.saved_queue_secs

    def test_busy_response_when_all_process_slots_are_held(self):
        server.CRACK_SLOTS = threading.BoundedSemaphore(1)
        server.CRACK_SLOTS.acquire()
        self.addCleanup(server.CRACK_SLOTS.release)

        result = server.run_cracknum("-fsp", "RNE", "1")
        self.assertIn("server is busy", result.lower())

    def test_process_slot_is_released_after_a_conversion(self):
        server.CRACK_SLOTS = threading.BoundedSemaphore(1)
        server._run_cracknum = lambda flag, rounding, value: "ok"

        self.assertEqual("ok", server.run_cracknum("-fsp", "RNE", "1"))
        self.assertTrue(server.CRACK_SLOTS.acquire(blocking=False))
        server.CRACK_SLOTS.release()

    def test_connection_beyond_the_thread_limit_gets_a_503(self):
        # The bound is read when the server is constructed, so shrink it first.
        saved_limit = server.MAX_HTTP_THREADS
        server.MAX_HTTP_THREADS = 1
        try:
            srv = server.BoundedThreadingHTTPServer(("127.0.0.1", 0), server.Handler)
        finally:
            server.MAX_HTTP_THREADS = saved_limit
        self.addCleanup(srv.server_close)

        worker = threading.Thread(target=srv.serve_forever, daemon=True)
        worker.start()
        self.addCleanup(srv.shutdown)

        host, port = srv.server_address

        # An idle client that never sends a request pins the only worker thread.
        idle = socket.create_connection((host, port), timeout=5)
        self.addCleanup(idle.close)
        deadline = time.monotonic() + 5
        while srv._request_slots._value > 0 and time.monotonic() < deadline:
            time.sleep(0.01)
        self.assertEqual(0, srv._request_slots._value, "worker slot was never taken")

        # The next connection must be turned away by the accept loop itself.
        extra = socket.create_connection((host, port), timeout=5)
        self.addCleanup(extra.close)
        reply = b""
        while not reply.endswith(b"Server busy\n"):
            chunk = extra.recv(4096)
            if not chunk:
                break
            reply += chunk
        self.assertTrue(reply.startswith(b"HTTP/1.1 503 "), reply)
        self.assertIn(b"Connection: close", reply)


if __name__ == "__main__":
    unittest.main()
