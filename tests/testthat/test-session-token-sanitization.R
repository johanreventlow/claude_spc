# tests/testthat/test-session-token-sanitization.R
# Tests for session token sanitization with SHA256 hashing

test_that("sanitize_session_token uses SHA256 with 8-character prefix", {
  # Test token
  test_token <- "abc123def456ghi789jkl012mno345pqr678stu901vwx234yz"

  sanitized <- sanitize_session_token(test_token)

  # Should return 8 characters
  expect_equal(nchar(sanitized), 8)

  # Should be consistent (same token = same hash)
  sanitized2 <- sanitize_session_token(test_token)
  expect_equal(sanitized, sanitized2)

  # Should be different from the original token
  expect_false(grepl(sanitized, test_token, fixed = TRUE))
})

test_that("sanitize_session_token uses SHA256 algorithm", {
  test_token <- "test_session_token_12345"

  # Calculate expected SHA256 hash
  expected_hash <- substr(digest::digest(test_token, algo = "sha256"), 1, 8)

  # Get sanitized token
  sanitized <- sanitize_session_token(test_token)

  # Should match SHA256 hash
  expect_equal(sanitized, expected_hash)
})

test_that("sanitize_session_token handles NULL tokens", {
  expect_equal(sanitize_session_token(NULL), "NO_SESSION")
})

test_that("sanitize_session_token handles empty tokens", {
  expect_equal(sanitize_session_token(""), "INVALID_SESSION")
  expect_equal(sanitize_session_token(character(0)), "NO_SESSION")
})

test_that("sanitize_session_token handles non-character input", {
  # Should handle conversion gracefully
  expect_equal(nchar(sanitize_session_token(12345)), 8)
})

test_that("Different tokens produce different hashes", {
  token1 <- "session_token_1"
  token2 <- "session_token_2"

  hash1 <- sanitize_session_token(token1)
  hash2 <- sanitize_session_token(token2)

  # Different tokens should produce different hashes
  expect_false(hash1 == hash2)

  # Both should be 8 characters
  expect_equal(nchar(hash1), 8)
  expect_equal(nchar(hash2), 8)
})

test_that("hash_session_token uses SHA256 with 8-character prefix", {
  # Source the function (it's in app_server_main.R but not exported)
  source("../../R/app_server_main.R")

  test_token <- "test_session_token_xyz"

  hashed <- hash_session_token(test_token)

  # Should return 8 characters
  expect_equal(nchar(hashed), 8)

  # Should match SHA256 hash
  expected_hash <- substr(digest::digest(test_token, algo = "sha256"), 1, 8)
  expect_equal(hashed, expected_hash)
})

test_that("hash_session_token handles NULL and invalid input", {
  source("../../R/app_server_main.R")

  expect_equal(hash_session_token(NULL), "unknown")
  expect_equal(hash_session_token(NA), "unknown")
})

test_that("SHA256 provides strong collision resistance", {
  # Generate 1000 different tokens
  tokens <- paste0("session_", 1:1000)

  # Hash all tokens
  hashes <- vapply(tokens, sanitize_session_token, character(1), USE.NAMES = FALSE)

  # All hashes should be unique (collision resistance)
  expect_equal(length(unique(hashes)), 1000)

  # All should be 8 characters
  expect_true(all(nchar(hashes) == 8))
})

test_that("Sanitized tokens are safe for logging", {
  # Simulate a real Shiny session token format
  real_token <- paste0(
    sample(c(letters, LETTERS, 0:9), 40, replace = TRUE),
    collapse = ""
  )

  sanitized <- sanitize_session_token(real_token)

  # Should not contain the original token
  expect_false(grepl(sanitized, real_token, fixed = TRUE))

  # Should be hexadecimal (SHA256 output)
  expect_true(grepl("^[a-f0-9]{8}$", sanitized))

  # Should be safe to log (no special characters)
  expect_true(grepl("^[a-zA-Z0-9]+$", sanitized))
})

test_that("SHA256 upgrade maintains backward-compatible length", {
  test_token <- "legacy_test_token"

  sanitized <- sanitize_session_token(test_token)

  # Should maintain 8-character prefix for log consistency
  expect_equal(nchar(sanitized), 8)

  # Should be compatible with existing log parsing
  # (8 hex characters are easily distinguishable in logs)
  expect_true(grepl("^[a-f0-9]{8}$", sanitized))
})
