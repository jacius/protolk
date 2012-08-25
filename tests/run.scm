
(register-feature! 'protolk-all-tests)

(load-relative "helpers")

(load-relative "../protolk-internal.scm")
(load-relative "../protolk-primitives.scm")
(load-relative "../protolk.scm")

(import protolk-internal protolk-primitives protolk)

(load-relative "primitives-spec.scm")
(load-relative "protolk-spec.scm")

(test-exit)
