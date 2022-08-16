m*&---------------------------------------------------------------------*
*& Report ZDEMO_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_report.

START-OF-SELECTION.

  TYPES: BEGIN OF ty_type,
           record(4)    TYPE c,
           item         TYPE i,
           debit_credit TYPE c,
           amount       TYPE i,
         END OF ty_type.

  DATA: it_tab   TYPE TABLE OF ty_type.


  it_tab = VALUE #(   ( record = 5888 item = 10 debit_credit = 'C' amount = 100 )
                      ( record = 5888 item = 10 debit_credit = 'D' amount = 50 )
                      ( record = 5888 item = 20 debit_credit = 'C' amount = 90 )
                      ( record = 5888 item = 30 debit_credit = 'D' amount = 200 )
                      ( record = 5888 item = 30 debit_credit = 'C' amount = 100 )
                      ( record = 5889 item = 10 debit_credit = 'C' amount = 100 )
                      ( record = 5889 item = 20 debit_credit = 'C' amount = 50 )
                      ( record = 5889 item = 20 debit_credit = 'D' amount = 100 )
                      ( record = 5889 item = 20 debit_credit = 'C' amount = 100 ) ).

*  cl_demo_output=>display( it_tab ).

  DATA: lv_total TYPE i.
  SORT it_tab ASCENDING BY record item.

  LOOP AT it_tab INTO DATA(wa_tab).
    IF wa_tab-debit_credit = 'D'.
      wa_tab-amount = wa_tab-amount * -1 .
      MODIFY it_tab FROM wa_tab INDEX sy-tabix.
    ENDIF.
    AT NEW record.
      ULINE.
      WRITE:/ 'Doc_Number', 'Doc_Item', 'Balance'.
    ENDAT.
    AT END OF item.
      SUM.
      WRITE:/ wa_tab-record, wa_tab-item, wa_tab-amount.
    ENDAT.
    AT END OF record.
      SUM.
      WRITE:/ 'Subtotal', wa_tab-amount.
      lv_total = lv_total + wa_tab-amount.
    ENDAT.
    AT LAST.
      ULINE.
      WRITE:/ 'Grand_Total', lv_total.
    ENDAT.
  ENDLOOP.