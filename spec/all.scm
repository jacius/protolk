
(register-feature! 'protolk-all-tests)

(load-relative "spec-helpers")

(load-relative "../protolk-internal.scm")
(load-relative "../protolk-primitives.scm")
(load-relative "../protolk-stdpob.scm")
(load-relative "../protolk.scm")

(import protolk-internal protolk-primitives protolk)

(load-relative "spec-helpers-spec.scm")
(load-relative "primitives-spec.scm")
(load-relative "core-spec.scm")
(load-relative "std-methods-spec.scm")
(load-relative "accessor-spec.scm")
(load-relative "encapsulation-spec.scm")
(load-relative "method-spec.scm")
(load-relative "stdpob-spec.scm")

(test-exit)
