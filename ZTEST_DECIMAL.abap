*&---------------------------------------------------------------------*
*& Report ZTEST_DECIMAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_decimal.

TYPES: BEGIN OF t_tab,
         sample_value(30) TYPE c,
       END OF t_tab.

TYPES: BEGIN OF t_number,
         text(30) TYPE c,
       END OF t_number.

DATA: i_tab  TYPE STANDARD TABLE OF t_tab,
      wa_tab TYPE t_tab.

DATA: g_dcpfm TYPE usr01-dcpfm.
DATA: g_whole(30)   TYPE c,
      g_decimal(30) TYPE c,
      g_strlen      TYPE i,
      g_ndiv        TYPE i,
      g_nmod        TYPE i,
      g_delimiter   TYPE c,
      g_separator   TYPE c,
      name(30)      TYPE c.

INITIALIZATION.
  CLEAR: g_dcpfm,
         g_whole,
         g_decimal,
         g_strlen,
         g_ndiv,
         g_nmod,
         g_delimiter,
         g_separator.

  SELECTION-SCREEN BEGIN OF BLOCK b1.
*parameter: p_value(30) type c.
  SELECT-OPTIONS: s_value FOR name NO INTERVALS OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK b1.
*At selection-screen
START-OF-SELECTION.

  SELECT SINGLE dcpfm
  INTO g_dcpfm
  FROM usr01
  WHERE bname = sy-uname.
  IF sy-subrc NE 0.
    g_dcpfm = space. "by default
  ENDIF.

  LOOP AT s_value.
    SPLIT s_value-low AT '.' INTO g_whole
                                     g_decimal.
    g_strlen = strlen( g_whole ).
    g_ndiv   = g_strlen DIV 3.
    g_nmod   = g_strlen MOD 3.

    CASE g_dcpfm.
      WHEN space.
        g_delimiter = '.'.
        g_separator = ','.
        PERFORM change_quantity USING g_delimiter
                                      g_separator
                            CHANGING s_value-low.

        CONCATENATE s_value-low
                    g_decimal
        INTO s_value-low
        SEPARATED BY g_separator.

      WHEN 'X'.
        g_delimiter = ','.
        g_separator = '.'.
        PERFORM change_quantity USING g_delimiter
                                      g_separator
                            CHANGING s_value-low.
        CONCATENATE s_value-low
                    g_decimal
        INTO s_value-low
        SEPARATED BY g_separator.

      WHEN 'Y'.
        g_delimiter = space.
        g_separator = ','.
        PERFORM change_quantity USING g_delimiter
                                      g_separator
                            CHANGING s_value-low.
        CONCATENATE s_value-low
                    g_decimal
        INTO s_value-low
        SEPARATED BY g_separator.
      WHEN OTHERS.
    ENDCASE.
    MODIFY s_value. "from wa_tab.
  ENDLOOP.

end-of-selection.
  WRITE: sy-uname.
  WRITE: g_dcpfm.

  LOOP AT s_value. "into wa_tab.
    WRITE: / s_value-low.
  ENDLOOP.

FORM change_quantity USING p_delimiter TYPE c
                                               p_separator TYPE c
                                      CHANGING p_sample_value.
  DATA: li_number  TYPE STANDARD TABLE OF t_number,
        lwa_number TYPE t_number,
        l_flag     TYPE c.
  DATA: l_temp              TYPE i,
        l_previous          TYPE i,
        l_previous_text(30) TYPE c.

  REFRESH: li_number.
  CLEAR:   lwa_number,
           l_previous,
           l_temp,
           l_flag,
           l_previous_text.

* initially *
  CLEAR: lwa_number,
         l_temp.
  IF NOT g_nmod IS INITIAL.
    lwa_number-text = g_whole(g_nmod).
    APPEND lwa_number TO li_number.
    l_temp = g_nmod.
  ENDIF.

  CLEAR: l_previous.
  DO g_ndiv TIMES.
    lwa_number-text = g_whole+l_temp(3).
    l_temp = l_previous + 3.
    l_previous = l_temp.
    APPEND lwa_number TO li_number.
    CLEAR: lwa_number.
  ENDDO.

  CLEAR: l_flag,
         l_previous_text.
  LOOP AT li_number INTO lwa_number.
    IF l_flag IS INITIAL.
      CONCATENATE l_previous_text
                  lwa_number-text
      INTO g_whole
      SEPARATED BY p_delimiter.

      REPLACE FIRST OCCURRENCE OF p_delimiter IN g_whole WITH space.
      CONDENSE g_whole NO-GAPS.

      l_previous_text = g_whole.
      l_flag = 'X'.
    ELSE.
      CONCATENATE l_previous_text
                  lwa_number-text
      INTO g_whole
      SEPARATED BY p_delimiter.
      l_previous_text = g_whole.

    ENDIF.
    p_sample_value = g_whole.
  ENDLOOP.

ENDFORM.