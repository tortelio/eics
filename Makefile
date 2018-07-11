.PHONY : clean

default:parsgen

compile:
	./rebar3 compile

ctest:
	./rebar3 ct

parsgen:
	./priv/abnfc -o "src/" priv/abnf/rfc3629.abnf
	./priv/abnfc -o "src/" priv/abnf/rfc3986.abnf
	./priv/abnfc -o "src/" priv/abnf/rfc4234.abnf
	./priv/abnfc -o "src/" priv/abnf/rfc4288.abnf
	./priv/abnfc -o "src/" priv/abnf/rfc5234.abnf
	./priv/abnfc -o "src/" priv/abnf/rfc5545.abnf
	./priv/abnfc -o "src/" priv/abnf/rfc5646.abnf
clean:
	rm -rf _build