PROGRAM fibonacci;

VAR upperBoundary, previous, current, next, first, second: INTEGER;
BEGIN
	READ(upperBoundary);
	previous := 1;
	current := 1;
	next := 1;
	first := 0;
	second := 1;

	WRITE(first);
	WRITE(second);
	WHILE upperBoundary DO BEGIN
		WRITE(current);
		next := current + previous;
		previous := current;
		current := next;
		upperBoundary := upperBoundary - 1;
	END;
END.

