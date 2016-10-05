tasks.erl - exports function task/1:

-spec task(integer()) -> ok | {error, not_implemented}.

Calling "task(N)" returns answer for Nth task (from 1 to 5):

1> c(tasks).
{ok,tasks}
2> tasks:task(1).
233168
3> tasks:task(2).
4613732
4> tasks:task(3).
6857
5> tasks:task(4).
906609
6> tasks:task(5).
232792560
7>

-------------------------------------------------------------------------------

webserver/ - task #6:

To compile:

$ make

To run application :

$ run

or

$ run /some/output/file

To make test POST request with Test.xml:

$ test

-------------------------------------------------------------------------------

seq_calc/ - task #7 - look README inside

-------------------------------------------------------------------------------

gtin_14_validator.erl - task #8 - exports function validate/1:

-spec validate ({gtin, Value :: iolist() | binary()}) -> ok | {error, term()}.

1> c(gtin_14_validator).
{ok,gtin_14_validator}
2> gtin_14_validator:validate({gtin, <<"10384478861804">>}).
ok
3> gtin_14_validator:validate({gtin, <<"10012345123457">>}).
ok
4> gtin_14_validator:validate({gtin, <<"00012345600010">>}).
{error,invalid_check_digit}
5>

-------------------------------------------------------------------------------

sql/ - task #9
