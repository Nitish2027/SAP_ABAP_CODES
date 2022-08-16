*&---------------------------------------------------------------------*
*& Include          /MIBA/ZREF_VS_DEL_TOP
*&---------------------------------------------------------------------*

TABLES: bkpf, mseg, makt.

TYPES: BEGIN OF ty_final,
         bukrs      TYPE bkpf-bukrs,
         belnr      TYPE bkpf-belnr,
         blart      TYPE bkpf-blart,
         gjahr      TYPE bkpf-gjahr,
         xblnr      TYPE bkpf-xblnr,
         budat      TYPE bkpf-budat,
         awkey      TYPE bkpf-awkey,
         delnt      TYPE mseg-xblnr_mkpf,
         matnr      TYPE mseg-matnr,
         dmbtr      TYPE mseg-dmbtr,
         bwart      TYPE mseg-bwart,
         zeile      TYPE mseg-zeile,
         cpudt_mkpf TYPE mseg-cpudt_mkpf,
         budat_mkpf TYPE mseg-budat_mkpf,
         erfmg      TYPE mseg-erfmg,
         erfme      TYPE mseg-erfme,
         ebeln      TYPE mseg-ebeln,
         charg      TYPE mseg-charg,
         maktx      TYPE makt-maktx,
       END OF ty_final,
       ty_t_final TYPE STANDARD TABLE OF ty_final WITH DEFAULT KEY.

DATA: gt_final TYPE ty_t_final.