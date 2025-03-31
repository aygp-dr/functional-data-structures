"""
Simple Python test file to ensure pytest is working properly with Hy
"""
import pytest

def test_py_basic():
    """Basic test to ensure pytest is working"""
    assert 1 == 1
    assert [1, 2, 3] == [1, 2, 3]
    assert "b" in "abc"