.PHONY: test
test:
	clojure -T:build test

run:
	clj -A:run-m
	
