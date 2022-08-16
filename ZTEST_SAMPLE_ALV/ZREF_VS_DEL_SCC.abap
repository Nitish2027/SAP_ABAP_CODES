*&---------------------------------------------------------------------*
*& Include          /MIBA/ZREF_VS_DEL_SCC
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: so_bukrs FOR bkpf-bukrs OBLIGATORY NO INTERVALS,
                                  so_belnr FOR bkpf-belnr NO INTERVALS,
                                  so_blart FOR bkpf-blart NO INTERVALS,
                                  so_gjahr FOR bkpf-gjahr OBLIGATORY NO INTERVALS,
                                  so_xblnr FOR bkpf-xblnr NO INTERVALS,
                                  so_delnt FOR mseg-xblnr_mkpf NO INTERVALS,
                                  so_matnr FOR mseg-matnr NO INTERVALS,
                                  so_budat FOR bkpf-budat.

SELECTION-SCREEN: END OF BLOCK b1.