*&---------------------------------------------------------------------*
*& Report ztest_string_split_by_word
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_string_split_by_word.

DATA(lv_string) = 'Put simply, all of the different types and kinds of paragraphs simply involve layering on a different purpose or intent. When students have the right foundation, it’s just that simple. What are you trying to achieve in this paragraph.'.

DATA: it_tab TYPE TABLE OF string,
      it_words TYPE TABLE OF string,
      wa_tab(255) TYPE C,
      lv_temp(255) TYPE C,
      lv_len_line TYPE I VALUE 0,
      lv_len_word TYPE I VALUE 0,
      lv_len_total TYPE I VALUE 0.


SPLIT lv_string AT space INTO TABLE it_words.

LOOP AT it_words INTO wa_tab.

  lv_len_word = STRLEN( wa_tab ).
  lv_len_line = STRLEN( lv_temp ).
  lv_len_total = lv_len_word + lv_len_line.
  IF lv_len_total GT 50.
    APPEND lv_temp TO it_tab.
    lv_temp = wa_tab.
  ELSE.
    IF lv_temp IS INITIAL.
      lv_temp = wa_tab.
    ELSE.
      CONCATENATE lv_temp wa_tab INTO lv_temp SEPARATED BY space.
    ENDIF.
  ENDIF.

ENDLOOP.

APPEND lv_temp TO it_tab.


cl_demo_output=>display( it_tab ).