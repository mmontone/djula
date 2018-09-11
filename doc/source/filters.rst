.. highlightlang:: html+django
		   
Filters
=======

Overview
--------

You can modify variables for display by using **filters**.

Filters look like this: ``{{ name|lower }}``. This displays the value of the
``{{ name }}`` variable after being filtered through the :tfilter:`lower`
filter, which converts text to lowercase. Use a pipe (``|``) to apply a filter.

Filters can be "chained." The output of one filter is applied to the next.
``{{ text|escape|linebreaks }}`` is a common idiom for escaping text contents,
then converting line breaks to ``<p>`` tags.

Some filters take arguments. A filter argument looks like this: ``{{
bio|truncatewords:30 }}``. This will display the first 30 words of the ``bio``
variable.

Filter arguments that contain spaces must be quoted; for example, to join a
list with commas and spaced you'd use ``{{ list|join:", " }}``.

Djula provides about thirty built-in template filters. You can read all about
them in the :ref:`built-in filter reference <ref-templates-builtins-filters>`.
To give you a taste of what's available, here are some of the more commonly
used template filters:

List of filters
---------------

.. contents:: Filters
   :local:

.. templatefilter:: add

add
^^^

Adds the argument to the value.

For example::

    {{ value|add:2 }}

If ``value`` is ``4``, then the output will be ``6``.

.. templatefilter:: addslashes

addslashes
^^^^^^^^^^

Adds slashes before quotes. Useful for escaping strings in CSV, for example.

For example::

    {{ value|addslashes }}

If ``value`` is ``"I'm using Djula"``, the output will be
``"I\'m using Djula"``.

.. templatefilter:: capfirst

capfirst
^^^^^^^^

Capitalizes the first character of the value. If the first character is not
a letter, this filter has no effect.

For example::

    {{ value|capfirst }}

If ``value`` is ``"djula"``, the output will be ``"Djula"``.

.. templatefilter:: center

..
   center
   ^^^^^^

   Centers the value in a field of a given width.

   For example::

       "{{ value|center:"15" }}"

   If ``value`` is ``"Djula"``, the output will be ``"     Djula    "``.

.. templatefilter:: cut

cut
^^^

Removes all values of arg from the given string.

For example::

    {{ value|cut:" " }}

If ``value`` is ``"String with spaces"``, the output will be
``"Stringwithspaces"``.

.. templatefilter:: date

date
^^^^

Formats a date

Example::
  {{ date-today | date }}

A LOCAL-TIME format spec can be provided::

  {{ date-today | date ()

.. templatefilter:: time

time		    
^^^^

Formats a time

Example::

  {{ time-now | time }}

.. templatefilter:: datetime  

datetime		    
^^^^^^^^

Formats a date and time

Example::

  {{ time-now | datetime }}


.. templatefilter:: default

default
^^^^^^^

If value evaluates to ``False``, uses the given default. Otherwise, uses the
value.

For example::

    {{ value|default "nothing" }}

If ``value`` is ``""`` (the empty string), the output will be ``nothing``.

.. templatefilter:: default_if_none

.. templatefilter:: sort

sort
^^^^

Takes a list and returns that list sorted.

For example::

    {{ list | sort }}

reverse
^^^^^^^

Takes a list and returns that list reversed.

For example::

    {{ list | reverse }}  

..
   divisibleby
   ^^^^^^^^^^^

   Returns ``True`` if the value is divisible by the argument.

   For example::

       {{ value|divisibleby:"3" }}

   If ``value`` is ``21``, the output would be ``True``.

..
   .. templatefilter:: escape

   escape
   ^^^^^^

   Escapes a string's HTML. Specifically, it makes these replacements:

   * ``<`` is converted to ``&lt;``
   * ``>`` is converted to ``&gt;``
   * ``'`` (single quote) is converted to ``&#39;``
   * ``"`` (double quote) is converted to ``&quot;``
   * ``&`` is converted to ``&amp;``

   The escaping is only applied when the string is output, so it does not matter
   where in a chained sequence of filters you put ``escape``: it will always be
   applied as though it were the last filter. If you want escaping to be applied
   immediately, use the :tfilter:`force-escape` filter.

   Applying ``escape`` to a variable that would normally have auto-escaping
   applied to the result will only result in one round of escaping being done. So
   it is safe to use this function even in auto-escaping environments. If you want
   multiple escaping passes to be applied, use the :tfilter:`force-escape` filter.

   For example, you can apply ``escape`` to fields when :ttag:`autoescape` is off::

       {% autoescape off %}
	   {{ title|escape }}
       {% endautoescape %}

   .. templatefilter:: escapejs

   escapejs
   ^^^^^^^^

   Escapes characters for use in JavaScript strings. This does *not* make the
   string safe for use in HTML, but does protect you from syntax errors when using
   templates to generate JavaScript/JSON.

   For example::

       {{ value|escapejs }}

   If ``value`` is ``"testing\r\njavascript \'string" <b>escaping</b>"``,
   the output will be ``"testing\\u000D\\u000Ajavascript \\u0027string\\u0022 \\u003Cb\\u003Eescaping\\u003C/b\\u003E"``.

   .. templatefilter:: filesizeformat

   filesizeformat
   ^^^^^^^^^^^^^^

   Formats the value like a 'human-readable' file size (i.e. ``'13 KB'``,
   ``'4.1 MB'``, ``'102 bytes'``, etc).

   For example::

       {{ value|filesizeformat }}

   If ``value`` is 123456789, the output would be ``117.7 MB``.

   .. admonition:: File sizes and SI units

       Strictly speaking, ``filesizeformat`` does not conform to the International
       System of Units which recommends using KiB, MiB, GiB, etc. when byte sizes
       are calculated in powers of 1024 (which is the case here). Instead, Djula
       uses traditional unit names (KB, MB, GB, etc.) corresponding to names that
       are more commonly used.

.. templatefilter:: first

first
^^^^^

Returns the first item in a list.

For example::

    {{ value|first }}

If ``value`` is the list ``("a" "b" "c")``, the output will be ``"a"``.

.. templatefilter:: join

join
^^^^

Joins a list with a string.

For example::

    {{ value|join:" // " }}

If ``value`` is the list ``("a" "b" "c")``, the output will be the string
``"a // b // c"``.

.. templatefilter:: last

last
^^^^

Returns the last item in a list.

For example::

    {{ value|last }}

If ``value`` is the list ``("a" "b" "c" "d")``, the output will be the
string ``"d"``.

.. templatefilter:: length

length
^^^^^^

Returns the length of the value. This works for both strings and lists.

For example::

    {{ value|length }}

If ``value`` is ``("a" "b" "c" "d")`` or ``"abcd"``, the output will be
``4``.

..
   .. templatefilter:: length_is

   length_is
   ^^^^^^^^^

   Returns ``True`` if the value's length is the argument, or ``False`` otherwise.

   For example::

       {{ value|length_is:"4" }}

   If ``value`` is ``['a', 'b', 'c', 'd']`` or ``"abcd"``, the output will be
   ``True``.

   .. templatefilter:: linebreaks

linebreaks
^^^^^^^^^^

Replaces line breaks in plain text with appropriate HTML; a single
newline becomes an HTML line break (``<br />``) and a new line
followed by a blank line becomes a paragraph break (``</p>``).

For example::

    {{ value|linebreaks }}

If ``value`` is ``Joel\nis a slug``, the output will be ``<p>Joel<br />is a
slug</p>``.

.. templatefilter:: linebreaksbr

linebreaksbr
^^^^^^^^^^^^

Converts all newlines in a piece of plain text to HTML line breaks
(``<br />``).

For example::

    {{ value|linebreaksbr }}

If ``value`` is ``Joel\nis a slug``, the output will be ``Joel<br />is a
slug``.

.. templatefilter:: linenumbers

..
   linenumbers
   ^^^^^^^^^^^

   Displays text with line numbers.

   For example::

       {{ value|linenumbers }}

   If ``value`` is::

       one
       two
       three

   the output will be::

       1. one
       2. two
       3. three

   .. templatefilter:: ljust

   ljust
   ^^^^^

   Left-aligns the value in a field of a given width.

   **Argument:** field size

   For example::

       "{{ value|ljust:"10" }}"

   If ``value`` is ``Djula``, the output will be ``"Djula    "``.

.. templatefilter:: lower

lower
^^^^^

Converts a string into all lowercase.

For example::

    {{ value|lower }}

If ``value`` is ``Still MAD At Yoko``, the output will be
``still mad at yoko``.

.. templatefilter:: make_list

..
   make_list
   ^^^^^^^^^

   Returns the value turned into a list. For a string, it's a list of characters.
   For an integer, the argument is cast into an unicode string before creating a
   list.

   For example::

       {{ value|make_list }}

   If ``value`` is the string ``"Joel"``, the output would be the list
   ``['J', 'o', 'e', 'l']``. If ``value`` is ``123``, the output will be the
   list ``['1', '2', '3']``.

   .. templatefilter:: phone2numeric

   phone2numeric
   ^^^^^^^^^^^^^

   Converts a phone number (possibly containing letters) to its numerical
   equivalent.

   The input doesn't have to be a valid phone number. This will happily convert
   any string.

   For example::

       {{ value|phone2numeric }}

   If ``value`` is ``800-COLLECT``, the output will be ``800-2655328``.

   .. templatefilter:: pluralize

   pluralize
   ^^^^^^^^^

   Returns a plural suffix if the value is not 1. By default, this suffix is
   ``'s'``.

   Example::

       You have {{ num_messages }} message{{ num_messages|pluralize }}.

   If ``num_messages`` is ``1``, the output will be ``You have 1 message.``
   If ``num_messages`` is ``2``  the output will be ``You have 2 messages.``

   For words that require a suffix other than ``'s'``, you can provide an alternate
   suffix as a parameter to the filter.

   Example::

       You have {{ num_walruses }} walrus{{ num_walruses|pluralize:"es" }}.

   For words that don't pluralize by simple suffix, you can specify both a
   singular and plural suffix, separated by a comma.

   Example::

       You have {{ num_cherries }} cherr{{ num_cherries|pluralize:"y,ies" }}.

   .. note:: Use :ttag:`blocktrans` to pluralize translated strings.

   .. templatefilter:: pprint

   pprint
   ^^^^^^

   A wrapper around :func:`pprint.pprint` -- for debugging, really.

   .. templatefilter:: random

   random
   ^^^^^^

   Returns a random item from the given list.

   For example::

       {{ value|random }}

   If ``value`` is the list ``['a', 'b', 'c', 'd']``, the output could be ``"b"``.

   .. templatefilter:: removetags

   removetags
   ^^^^^^^^^^

   Removes a space-separated list of [X]HTML tags from the output.

   For example::

       {{ value|removetags:"b span"|safe }}

   If ``value`` is ``"<b>Joel</b> <button>is</button> a <span>slug</span>"`` the
   output will be ``"Joel <button>is</button> a slug"``.

   Note that this filter is case-sensitive.

   If ``value`` is ``"<B>Joel</B> <button>is</button> a <span>slug</span>"`` the
   output will be ``"<B>Joel</B> <button>is</button> a slug"``.

   .. templatefilter:: rjust

   rjust
   ^^^^^

   Right-aligns the value in a field of a given width.

   **Argument:** field size

   For example::

       "{{ value|rjust:"10" }}"

   If ``value`` is ``Djula``, the output will be ``"    Djula"``.

.. templatefilter:: safe

safe
^^^^

Marks a string as not requiring further HTML escaping prior to output. When
autoescaping is off, this filter has no effect.

.. note::

    If you are chaining filters, a filter applied after ``safe`` can
    make the contents unsafe again. For example, the following code
    prints the variable as is, unescaped:

    .. code-block:: html+django

        {{ var|safe|escape }}

.. templatefilter:: safeseq

..
   safeseq
   ^^^^^^^

   Applies the :tfilter:`safe` filter to each element of a sequence. Useful in
   conjunction with other filters that operate on sequences, such as
   :tfilter:`join`. For example::

       {{ some_list|safeseq|join:", " }}

   You couldn't use the :tfilter:`safe` filter directly in this case, as it would
   first convert the variable into a string, rather than working with the
   individual elements of the sequence.

.. templatefilter:: slice

slice
^^^^^

Returns a slice of a sequence (i.e. lists, vectors, strings)

Uses the Common Lisp ``cl-slice`` library.

Syntax::

  {{ seq | slice: slices }}

Each ``slice`` selects a subset of subscripts along the corresponding axis.

* A nonnegative integer selects the corresponding index, while a negative integer selects an index counting backwards from the last index::
    
  {{ list | slice: 4 }}

if the list is ``(1 2 3 4 5 6)`` it will output ``(5)``

* ``(start . end)`` to select a range.  When ``end`` is ``NIL``, the last index is included.
Each boundary is resolved according to the other rules if applicable, so you can use negative integers::
    
  {{ string | slice: (0 . 5) }}
  {{ string | slice: (5 . nil) }}

if the string is ``"Hello world"`` is will output ``Hello`` and ``world``.

.. templatefilter:: slugify

..
   slugify
   ^^^^^^^

   Converts to lowercase, removes non-word characters (alphanumerics and
   underscores) and converts spaces to hyphens. Also strips leading and trailing
   whitespace.

   For example::

       {{ value|slugify }}

   If ``value`` is ``"Joel is a slug"``, the output will be ``"joel-is-a-slug"``.

.. templatefilter:: format

format
^^^^^^

Formats the variable according to the argument, a string formatting specifier.
This specifier uses Common Lisp string formatting syntax

For example::

    {{ value | format:"~:d" }}

If ``value`` is ``1000000``, the output will be ``1,000,000``.

.. templatefilter:: striptags

..
   striptags
   ^^^^^^^^^

   Makes all possible efforts to strip all [X]HTML tags.

   For example::

       {{ value|striptags }}

   If ``value`` is ``"<b>Joel</b> <button>is</button> a <span>slug</span>"``, the
   output will be ``"Joel is a slug"``.

   .. admonition:: No safety guarantee

       Note that ``striptags`` doesn't give any guarantee about its output being
       entirely HTML safe, particularly with non valid HTML input. So **NEVER**
       apply the ``safe`` filter to a ``striptags`` output.
       If you are looking for something more robust, you can use the ``bleach``
       Python library, notably its `clean`_ method.

   .. _clean: http://bleach.readthedocs.org/en/latest/clean.html

   .. templatefilter:: time

time
^^^^

Formats a time according to the given format.

For example::

    {{ value | time }}

..
   .. templatefilter:: timesince

   timesince
   ^^^^^^^^^

   Formats a date as the time since that date (e.g., "4 days, 6 hours").

   Takes an optional argument that is a variable containing the date to use as
   the comparison point (without the argument, the comparison point is *now*).
   For example, if ``blog_date`` is a date instance representing midnight on 1
   June 2006, and ``comment_date`` is a date instance for 08:00 on 1 June 2006,
   then the following would return "8 hours"::

       {{ blog_date|timesince:comment_date }}

   Comparing offset-naive and offset-aware datetimes will return an empty string.

   Minutes is the smallest unit used, and "0 minutes" will be returned for any
   date that is in the future relative to the comparison point.

   .. templatefilter:: timeuntil

   timeuntil
   ^^^^^^^^^

   Similar to ``timesince``, except that it measures the time from now until the
   given date or datetime. For example, if today is 1 June 2006 and
   ``conference_date`` is a date instance holding 29 June 2006, then
   ``{{ conference_date|timeuntil }}`` will return "4 weeks".

   Takes an optional argument that is a variable containing the date to use as
   the comparison point (instead of *now*). If ``from_date`` contains 22 June
   2006, then the following will return "1 week"::

       {{ conference_date|timeuntil:from_date }}

   Comparing offset-naive and offset-aware datetimes will return an empty string.

   Minutes is the smallest unit used, and "0 minutes" will be returned for any
   date that is in the past relative to the comparison point.

   .. templatefilter:: title

   title
   ^^^^^

   Converts a string into titlecase by making words start with an uppercase
   character and the remaining characters lowercase. This tag makes no effort to
   keep "trivial words" in lowercase.

   For example::

       {{ value|title }}

   If ``value`` is ``"my FIRST post"``, the output will be ``"My First Post"``.

.. templatefilter:: truncatechars

truncatechars
^^^^^^^^^^^^^

Truncates a string if it is longer than the specified number of characters.
Truncated strings will end with the :cl:symbol:*ELLISION-STRING*, which defaults to "...".

**Argument:** Number of characters to truncate to

For example::

    {{ value|truncatechars:9 }}

If ``value`` is ``"Joel is a slug"``, the output will be ``"Joel i..."``.

.. templatefilter:: truncatechars_html

..
   truncatechars_html
   ^^^^^^^^^^^^^^^^^^

   .. versionadded:: 1.7

   Similar to :tfilter:`truncatechars`, except that it is aware of HTML tags. Any
   tags that are opened in the string and not closed before the truncation point
   are closed immediately after the truncation.

   For example::

       {{ value|truncatechars_html:9 }}

   If ``value`` is ``"<p>Joel is a slug</p>"``, the output will be
   ``"<p>Joel i...</p>"``.

   Newlines in the HTML content will be preserved.

   .. templatefilter:: truncatewords

   truncatewords
   ^^^^^^^^^^^^^

   Truncates a string after a certain number of words.

   **Argument:** Number of words to truncate after

   For example::

       {{ value|truncatewords:2 }}

   If ``value`` is ``"Joel is a slug"``, the output will be ``"Joel is ..."``.

   Newlines within the string will be removed.

   .. templatefilter:: truncatewords_html

   truncatewords_html
   ^^^^^^^^^^^^^^^^^^

   Similar to :tfilter:`truncatewords`, except that it is aware of HTML tags. Any
   tags that are opened in the string and not closed before the truncation point,
   are closed immediately after the truncation.

   This is less efficient than :tfilter:`truncatewords`, so should only be used
   when it is being passed HTML text.

   For example::

       {{ value|truncatewords_html:2 }}

   If ``value`` is ``"<p>Joel is a slug</p>"``, the output will be
   ``"<p>Joel is ...</p>"``.

   Newlines in the HTML content will be preserved.

   .. templatefilter:: unordered_list

   unordered_list
   ^^^^^^^^^^^^^^

   Recursively takes a self-nested list and returns an HTML unordered list --
   WITHOUT opening and closing <ul> tags.

   The list is assumed to be in the proper format. For example, if ``var``
   contains ``['States', ['Kansas', ['Lawrence', 'Topeka'], 'Illinois']]``, then
   ``{{ var|unordered_list }}`` would return::

       <li>States
       <ul>
	       <li>Kansas
	       <ul>
		       <li>Lawrence</li>
		       <li>Topeka</li>
	       </ul>
	       </li>
	       <li>Illinois</li>
       </ul>
       </li>

   Note: An older, more restrictive and verbose input format is also supported:
   ``['States', [['Kansas', [['Lawrence', []], ['Topeka', []]]], ['Illinois', []]]]``,

.. templatefilter:: upper

upper
^^^^^

Converts a string into all uppercase.

For example::

    {{ value|upper }}

If ``value`` is ``"Joel is a slug"``, the output will be ``"JOEL IS A SLUG"``.

.. templatefilter:: urlencode

urlencode
^^^^^^^^^

Escapes a value for use in a URL.

For example::

    {{ value|urlencode }}

If ``value`` is ``"http://www.example.org/foo?a=b&c=d"``, the output will be
``"http%3A//www.example.org/foo%3Fa%3Db%26c%3Dd"``.

An optional argument containing the characters which should not be escaped can
be provided.

If not provided, the '/' character is assumed safe. An empty string can be
provided when *all* characters should be escaped. For example::

    {{ value|urlencode:"" }}

If ``value`` is ``"http://www.example.org/"``, the output will be
``"http%3A%2F%2Fwww.example.org%2F"``.

..
   .. templatefilter:: urlize

   urlize
   ^^^^^^

   Converts URLs and email addresses in text into clickable links.

   This template tag works on links prefixed with ``http://``, ``https://``, or
   ``www.``. For example, ``http://goo.gl/aia1t`` will get converted but
   ``goo.gl/aia1t`` won't.

   It also supports domain-only links ending in one of the original top level
   domains (``.com``, ``.edu``, ``.gov``, ``.int``, ``.mil``, ``.net``, and
   ``.org``). For example, ``djulaproject.com`` gets converted.

   .. versionchanged:: 1.8

       Support for domain-only links that include characters after the top-level
       domain (e.g. ``djulaproject.com/`` and ``djulaproject.com/download/``)
       was added.

   Links can have trailing punctuation (periods, commas, close-parens) and leading
   punctuation (opening parens), and ``urlize`` will still do the right thing.

   Links generated by ``urlize`` have a ``rel="nofollow"`` attribute added
   to them.

   For example::

       {{ value|urlize }}

   If ``value`` is ``"Check out www.djulaproject.com"``, the output will be
   ``"Check out <a href="http://www.djulaproject.com"
   rel="nofollow">www.djulaproject.com</a>"``.

   In addition to web links, ``urlize`` also converts email addresses into
   ``mailto:`` links. If ``value`` is
   ``"Send questions to foo@example.com"``, the output will be
   ``"Send questions to <a href="mailto:foo@example.com">foo@example</a>"``.

   The ``urlize`` filter also takes an optional parameter ``autoescape``. If
   ``autoescape`` is ``True``, the link text and URLs will be escaped using
   Djula's built-in :tfilter:`escape` filter. The default value for
   ``autoescape`` is ``True``.

   .. note::

       If ``urlize`` is applied to text that already contains HTML markup,
       things won't work as expected. Apply this filter only to plain text.

   .. templatefilter:: urlizetrunc

   urlizetrunc
   ^^^^^^^^^^^

   Converts URLs and email addresses into clickable links just like urlize_, but truncates URLs
   longer than the given character limit.

   **Argument:** Number of characters that link text should be truncated to,
   including the ellipsis that's added if truncation is necessary.

   For example::

       {{ value|urlizetrunc:15 }}

   If ``value`` is ``"Check out www.djulaproject.com"``, the output would be
   ``'Check out <a href="http://www.djulaproject.com"
   rel="nofollow">www.djulapr...</a>'``.

   As with urlize_, this filter should only be applied to plain text.

   .. templatefilter:: wordcount

   wordcount
   ^^^^^^^^^

   Returns the number of words.

   For example::

       {{ value|wordcount }}

   If ``value`` is ``"Joel is a slug"``, the output will be ``4``.

   .. templatefilter:: wordwrap

   wordwrap
   ^^^^^^^^

   Wraps words at specified line length.

   **Argument:** number of characters at which to wrap the text

   For example::

       {{ value|wordwrap:5 }}

   If ``value`` is ``Joel is a slug``, the output would be::

       Joel
       is a
       slug

   .. templatefilter:: yesno

   yesno
   ^^^^^

   Maps values for true, false and (optionally) None, to the strings "yes", "no",
   "maybe", or a custom mapping passed as a comma-separated list, and
   returns one of those strings according to the value:

   For example::

       {{ value|yesno:"yeah,no,maybe" }}

   ==========  ======================  ==================================
   Value       Argument                Outputs
   ==========  ======================  ==================================
   ``True``                            ``yes``
   ``True``    ``"yeah,no,maybe"``     ``yeah``
   ``False``   ``"yeah,no,maybe"``     ``no``
   ``None``    ``"yeah,no,maybe"``     ``maybe``
   ``None``    ``"yeah,no"``           ``"no"`` (converts None to False
				       if no mapping for None is given)
   ==========  ======================  ==================================

Custom filters
--------------

TODO
