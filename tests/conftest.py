"""
Pytest configuration for Functional Data Structures tests.
"""
import pytest
import hy

# Register Hy marker
def pytest_configure(config):
    config.addinivalue_line("markers", "hy: mark test to run only with Hy support")