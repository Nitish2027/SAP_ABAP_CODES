*&---------------------------------------------------------------------*
*& Report ZDEMO_ROC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_roc.

START-OF-SELECTION.

*  DATA: p_num TYPE string VALUE 100.
*  TYPES: BEGIN OF ty_int,
*           num TYPE int2,
*         END OF ty_int.
*  DATA: lt_int  TYPE TABLE OF ty_int,
*        lt_int1 TYPE TABLE OF ty_int.
*
*  DATA: lv_flag TYPE int2.
*  DATA: lv_temp TYPE int2 VALUE 1.
*  DATA: text TYPE string VALUE 'Prime Numbers: '.
*
*  lt_int =  VALUE #( FOR i = 2 THEN i + 1 UNTIL i >= p_num
*                                 ( num = i )
*                        ).
*
*  LOOP AT lt_int INTO DATA(ls_int).
*
*    lv_flag = 0.
*
*    LOOP AT lt_int INTO DATA(ls_int1) WHERE num <= ls_int-num.
*
*      lv_temp = ls_int-num MOD ls_int1-num.
*
*      IF lv_temp = 0.
*        lv_flag = lv_flag + 1.
*      ENDIF.
*
*    ENDLOOP.
*
*    IF lv_flag = 1.
*      text = text && | { ls_int-num }| && |,| .
*    ENDIF.
*
*  ENDLOOP.
*
*  WRITE:/ text.

*  lt_int = VALUE #( BASE lt_int
*            ( LINES OF VALUE #( FOR i = 1 THEN i + 1 UNTIL i >= 100
*                               ( num = i ) ) )
*                      ).

*  DATA(lv_odd) = REDUCE string( INIT text = `Odd Numbers:`
*                                   FOR i = 0 THEN i + 1 UNTIL i >= 100
*                                   NEXT
*                                   text = COND #( LET j = i MOD 2 IN
*                                   WHEN j = 0
*                                   THEN text
*                                   ELSE text && | { i }| && |,|
*                                    )
*                                   ).
*  WRITE:/ lv_odd.

*  DATA(lt_int) = VALUE ty_int( FOR i = 1 THEN i + 1 UNTIL i > 100
*                                      ( num = i )
*                             ).

*
*DATA(lv_prime) = REDUCE string( INIT text1 = `Prime Numbers:`
*                                 FOR i = 2 THEN i + 1 UNTIL i >= 100
*                                 FOR j = 2 THEN j + 1 UNTIL j >= i
*                                 NEXT
*                                 text1 = COND #( LET k = i MOD j IN
*                                 WHEN k = 0
*                                 THEN text1
*                                 ELSE text1 && | { i }| && |,|
*                                  )
*                                 ).
*
*  DATA(lv_prime1) = REDUCE string( INIT text = `Prime Numbers1:`
*                                   FOR i = 2 THEN i + 1 UNTIL i >= 100
*                                   NEXT
*                                   text = REDUCE string( INIT lv_flag = 0
*                                                          FOR j = i - 1 THEN j - 1 UNTIL j < 2
*                                                          NEXT
*                                                          lv_flag = COND #( LET k = ( i MOD j ) IN
*                                                           WHEN k = 0
*                                                           THEN lv_flag + 1
*                                                           ELSE lv_flag
*                                    )
*                                   )
*                                  ).

*  WRITE:/ lv_prime.

*  cl_demo_output=>display( lt_int1 ).

*DATA(lv_prime) = REDUCE string(
*    INIT text1 = `Prime Numbers:`
*         mod0_count = 0
*    FOR i = 2 THEN i + 1 WHILE i <= 100
*    FOR j = 2 THEN j + 1 WHILE j < i
*    NEXT mod0_count = COND #( WHEN j = 2 AND i MOD j = 0 THEN 1
*                              WHEN j = 2 THEN 0
*                              WHEN i MOD j = 0 THEN mod0_count + 1
*                              ELSE mod0_count )
*         text1      = COND #( WHEN j = i - 1 AND mod0_count = 0
*                              THEN text1 && | { i }| && |,|
*                              ELSE text1  ) ).
*
*WRITE:/ lv_prime.