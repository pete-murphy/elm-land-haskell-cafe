# https://just.systems

@help:
    just --list

@run:
    npx elm-land server

@test-timeout TIMEOUT_SECONDS="90" *ARGS:
    # Run elm-test-rs with a timeout using Python (portable)
    python3 scripts/elm_test_timeout.py {{TIMEOUT_SECONDS}} {{ARGS}}

@test-timeout-bun TIMEOUT_SECONDS="90" *ARGS:
    # Run elm-test-rs with a timeout using Bun (if available)
    bun scripts/elm_test_timeout.js {{TIMEOUT_SECONDS}} {{ARGS}}
