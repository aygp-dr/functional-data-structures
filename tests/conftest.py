"""
Pytest configuration for Functional Data Structures tests.
"""
import pytest
import hy

# Register Hy marker
def pytest_configure(config):
    config.addinivalue_line("markers", "hy: mark test to run only with Hy support")

# Import Hy modules for testing
def pytest_collect_file(parent, path):
    if path.ext == ".hy":
        # Ensure Hy files are properly collected
        return pytest.Module.from_parent(parent, path=path)
    return None