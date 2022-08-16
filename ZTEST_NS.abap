*&---------------------------------------------------------------------*
*& Report ZTEST_NS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_ns.

TABLES: scarr.

SELECT-OPTIONS: var_a FOR scarr-carrname.
PARAMETERS: b AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.

  IF var_a-low = 'American Airline'.
    b = 'X'.
  ELSE.
    b = 'Y'.
  ENDIF.

  WRITE: b.