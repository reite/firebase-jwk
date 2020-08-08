# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- Retry functionality to allow retrying if first request for keys returns a error.
- Tests for KeyStore.

### Fixed

- Rewrote implementation to better deal with errors. Would previously deadlock on error in certain cases.
