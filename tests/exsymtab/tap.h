/*
 * Simple library to output tests using the Test Anything Protocol.
 * This is far from being a complete TAP producer. For that, see libtap.
 * For a discussion of the TAP output format, see
 * http://podwiki.hexten.net/TAP/TAP.html?page=TAP
 *
 */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

int N_tests_planned;
int N_tests_seen;
int N_passes;

void init_tap(int N_tests) {
	if (N_tests_planned != 0) {
		printf("# ignoring call to init_tap since tap has already been initialized\n");
		fflush(stdout);
		return;
	}
	if (N_tests  == 0 || N_tests < -1) {
		printf("# ignoring call to init_tap with disallowed number of tests: %d\n", N_tests);
		fflush(stdout);
		return;
	}
	N_tests_planned = N_tests;
	N_tests_seen = 0;
	N_passes = 0;
	if (N_tests_planned > 0) printf("1..%d\n", N_tests_planned);
	fflush(stdout);
}

void done_testing() {
	if (N_tests_planned > 0) {
		printf("# ignoring call to done_testing since the number of tests was specified\n");
		fflush(stdout);
		return;
	}
	N_tests_planned = N_tests_seen;
	printf("%d..%d\n", N_tests_planned, N_tests_planned);
	fflush(stdout);
}

void _ok(int boolean) {
	N_tests_seen++;
	if (boolean) N_passes++;
	else printf("not ");
	printf("ok %d - ", N_tests_seen);
	fflush(stdout);
}

void ok(int boolean, const char * description) {
	_ok(boolean);
	if (description != 0) printf("%s", description);
	printf("\n");
	fflush(stdout);
}

void diag(const char * to_say, ...) {
	va_list args;
	va_start(args, to_say);
	printf("# ");
	vprintf(to_say, args);
	printf("\n");
	va_end(args);
	fflush(stdout);
}

void pass(const char * description) {
	ok(1, description);
}

void fail(const char * description) {
	ok(0, description);
}

void is (int got, int expected, const char * description) {
	if (got == expected) pass(description);
	else {
		fail(description);
		diag("got %d but expected %d", got, expected);
	}
}

void is_i (int got, int expected, const char * description) {
	if (got == expected) pass(description);
	else {
		fail(description);
		diag("got %d but expected %d", got, expected);
	}
}

void is_s (const char * got, const char * expected, const char * description) {
	if (strcmp(got, expected) == 0) pass(description);
	else {
		fail(description);
		diag("got %s but expected %s", got, expected);
	}
}

void is_d (double got, double expected, const char * description) {
	if (got == expected) pass(description);
	else {
		fail(description);
		diag("got %f but expected %f", got, expected);
	}
}
