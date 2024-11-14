.. title:: clang-tidy - modernize-use-starts-ends-with

modernize-use-starts-ends-with
==============================

Checks for common roundabout ways to express ``starts_with`` and ``ends_with``
and suggests replacing with the simpler method when it is available. Notably, 
this will work with ``std::string`` and ``std::string_view``.

The check handles the following expressions:

==================================================== ===========================
Expression                                            Result
---------------------------------------------------- ---------------------------
``u.find(v) == 0``                                   ``u.starts_with(v)``
``u.rfind(v, 0) != 0``                               ``!u.starts_with(v)``
``u.compare(0, v.size(), v) == 0``                   ``u.starts_with(v)``
``u.substr(0, v.size()) == v``                       ``u.starts_with(v)``
``v == u.substr(0, v.size())``                       ``u.starts_with(v)``
``u.substr(0, v.size()) != v``                       ``!u.starts_with(v)``
``u.compare(u.size() - v.size(), v.size(), v) == 0`` ``u.ends_with(v)``
``u.rfind(v) == u.size() - v.size()``               ``u.ends_with(v)``
==================================================== ===========================

For example:

.. code-block:: c++

  std::string s = "...";
  if (s.starts_with("prefix")) { /* do something */ }
  if (s.ends_with("suffix")) { /* do something */ }

Notes:

* For the ``substr`` pattern, the check ensures that:

  * The substring starts at position 0
  * The length matches exactly the compared string's length
  * The length is a constant value

* Non-matching cases (will not be transformed):

  * ``s.substr(1, 5) == "hello"``     // Non-zero start position
  * ``s.substr(0, 4) == "hello"``     // Length mismatch
  * ``s.substr(0, len) == "hello"``   // Non-constant length