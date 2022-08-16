*&---------------------------------------------------------------------*
*& Include          /MIBA/ZREF_VS_DEL_CLS
*&---------------------------------------------------------------------*

CLASS lcl_class DEFINITION.

    PUBLIC SECTION.
  
      METHODS:
        get_data,
        display.
  
    PROTECTED SECTION.
  
    PRIVATE SECTION.
      DATA: lo_alv  TYPE REF TO cl_salv_table,
            lo_func TYPE REF TO cl_salv_functions.
  
      DATA: gr_layout TYPE REF TO cl_salv_layout.
      DATA: key TYPE salv_s_layout_key.
  
  ENDCLASS.
  
  CLASS lcl_class IMPLEMENTATION.
  
    METHOD get_data.
  
      TYPES: lr_awkey TYPE RANGE OF awkey,
             lr_ebeln TYPE RANGE OF ebeln.
  
      SELECT bkpf~bukrs,
                    bkpf~belnr,
                    bkpf~blart,
                    bkpf~gjahr,
                    bkpf~xblnr,
                    bkpf~budat,
                    bkpf~awkey
      FROM bkpf
      INTO TABLE @DATA(lt_bkpf)
      WHERE bukrs IN @so_bukrs
          AND  belnr IN @so_belnr
          AND  gjahr IN @so_gjahr
          AND xblnr IN @so_xblnr
          AND budat IN @so_budat
          AND blart IN @so_blart.
  
      IF sy-subrc = 0.
  
        SORT lt_bkpf BY awkey.
  
        DATA(lr_awkey) = VALUE lr_awkey(
                                        FOR wa IN lt_bkpf (
                                                  sign = 'I'
                                                  option = 'EQ'
                                                  low = wa-awkey+0(10)
                                          )
                                        ).
  
        TRY.
  
            SELECT
                          belnr,
                          ebeln
            FROM rseg
            INTO TABLE @DATA(lt_rseg)
            WHERE  belnr IN @lr_awkey.
            IF sy-subrc = 0.
              SORT lt_rseg BY ebeln.
  
              DATA(lr_ebeln) = VALUE lr_ebeln(
                                              FOR wa1 IN lt_rseg (
                                                        sign = 'I'
                                                        option = 'EQ'
                                                        low = wa1-ebeln
                                                )
                                              ).
            ELSE.
              MESSAGE 'No Data Found RSEG Table for given input parameters' TYPE 'S' DISPLAY LIKE 'E'.
              LEAVE LIST-PROCESSING.
            ENDIF.
  
          CATCH cx_sy_open_sql_db INTO DATA(exref).
            DATA(msgtxt) = exref->get_text( ).
  
            MESSAGE msgtxt TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE LIST-PROCESSING.
  
        ENDTRY.
  
        TRY.
  
            SELECT
                          mseg~matnr,
                          mseg~dmbtr,
                          mseg~bwart,
                          mseg~mblnr,
                          mseg~zeile,
                          mseg~xblnr_mkpf,
                          mseg~cpudt_mkpf,
                          mseg~budat_mkpf,
                          mseg~erfmg,
                          mseg~erfme,
                          mseg~ebeln,
                          mseg~charg,
                          makt~maktx
            FROM mseg
            INNER JOIN makt
            ON mseg~matnr = makt~matnr
            AND spras = 'E'
            INTO TABLE @DATA(lt_mseg)
            WHERE mseg~matnr IN @so_matnr
                   AND mseg~mblnr IN @lr_ebeln
                   AND mseg~xblnr_mkpf IN @so_delnt.
  
            IF sy-subrc = 0.
              SORT lt_mseg BY mblnr.
            ELSE.
              MESSAGE 'No Data found in MSEG Table for given input parameters' TYPE 'S' DISPLAY LIKE 'E'.
              LEAVE LIST-PROCESSING.
            ENDIF.
  
          CATCH cx_sy_open_sql_db INTO exref.
            msgtxt = exref->get_text( ).
  
            MESSAGE msgtxt TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE LIST-PROCESSING.
  
        ENDTRY.
  
        gt_final = VALUE ty_t_final(
                                    FOR wa_bkpf IN lt_bkpf
                                    FOR wa_rseg IN lt_rseg FROM line_index( lt_rseg[ belnr = wa_bkpf-awkey+0(10) ] )
                                    WHERE ( belnr = wa_bkpf-awkey+0(10) )
                                    FOR wa_mseg IN lt_mseg FROM line_index( lt_mseg[ mblnr = wa_rseg-ebeln ] )
                                    WHERE ( mblnr = wa_rseg-ebeln )
                                    LET ls_final = VALUE ty_final(
                                           bukrs      = wa_bkpf-bukrs
                                           belnr      = wa_bkpf-belnr
                                           blart      = wa_bkpf-blart
                                           gjahr      = wa_bkpf-gjahr
                                           xblnr      = wa_bkpf-xblnr
                                           budat      = wa_bkpf-budat
                                           awkey      = wa_bkpf-awkey
                                           delnt      = wa_mseg-xblnr_mkpf
                                           matnr      = wa_mseg-matnr
                                           dmbtr      = wa_mseg-dmbtr
                                           bwart      = wa_mseg-bwart
                                           zeile      = wa_mseg-zeile
                                           cpudt_mkpf = wa_mseg-cpudt_mkpf
                                           budat_mkpf = wa_mseg-budat_mkpf
                                           erfmg      = wa_mseg-erfmg
                                           erfme      = wa_mseg-erfme
                                           ebeln      = wa_mseg-ebeln
                                           charg      = wa_mseg-charg
                                           maktx      = wa_mseg-maktx
                                    )
                               IN ( CORRESPONDING #( BASE ( ls_final ) wa_bkpf ) )
                              ).
  
        IF gt_final IS INITIAL.
          MESSAGE 'Change selection parameters! No Data Found!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.
  
      ELSE.
  
        MESSAGE 'Change selection parameters! No Data Found!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
    ENDMETHOD.
  
    METHOD display.
  
      TRY.
          cl_salv_table=>factory(
          IMPORTING
          r_salv_table = lo_alv
          CHANGING
          t_table = gt_final
          ).
  
          lo_func = lo_alv->get_functions( ).
          lo_func->set_all( abap_true ).
  
          gr_layout = lo_alv->get_layout( ).
          key-report = sy-repid.
          gr_layout->set_key( key ).
          gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  
        CATCH cx_salv_msg INTO DATA(lx_msg).
          cl_demo_output=>display( lx_msg ).
  
      ENDTRY.
  
      lo_alv->display( ).
  
    ENDMETHOD.
  
  ENDCLASS.