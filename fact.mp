PROGRAM fact;

VAR f, n: INTEGER;

BEGIN
	READ(n);
	IF n THEN BEGIN
		f := n;
		n := n - 1;
		WHILE n DO BEGIN
			f := n * f;
			n := n - 1;
		END;
	END
	ELSE BEGIN
		f := 0;
	END;
	WRITE(f);
END.
