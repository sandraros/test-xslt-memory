REPORT zzsro_xslt_memory.

CLASS ltc_app DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION MEDIUM.

  PRIVATE SECTION.
    METHODS xslt FOR TESTING RAISING cx_static_check.
    METHODS st   FOR TESTING RAISING cx_static_check.

    METHODS setup.

    TYPES:
      BEGIN OF ts_year,
        year     TYPE n LENGTH 4,
        amount_c TYPE p LENGTH 9 DECIMALS 2,
        amount_j TYPE p LENGTH 9 DECIMALS 2,
        amount   TYPE p LENGTH 9 DECIMALS 2,
      END OF ts_year.
    TYPES tt_year TYPE STANDARD TABLE OF ts_year WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_prps,
        posid         TYPE c LENGTH 24,
        text          TYPE c LENGTH 40,
        prog_year     TYPE n LENGTH 4,
        prart         TYPE c LENGTH 43,
        bdg_type      TYPE c LENGTH 4,
        dir           TYPE c LENGTH 9,
        section       TYPE c LENGTH 4,
        fincode_num   TYPE c LENGTH 10,
        fincode_short TYPE c LENGTH 20,
        fincode_long  TYPE c LENGTH 40,
        bdgserv_num   TYPE n LENGTH 8,
        bdgserv_text  TYPE c LENGTH 40,
        fistl_ge      TYPE c LENGTH 16,
        fistl_te      TYPE c LENGTH 16,
        kostl         TYPE c LENGTH 10,
        amount_d      TYPE p LENGTH 6 DECIMALS 2,
        amount_v      TYPE p LENGTH 6 DECIMALS 2,
        amount_p      TYPE p LENGTH 6 DECIMALS 2,
        status        TYPE c LENGTH 4,
        year          TYPE tt_year,
      END OF ts_prps.
    TYPES tt_prps TYPE STANDARD TABLE OF ts_prps WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_equi,
        main  TYPE c LENGTH 3,
        tplnr TYPE c LENGTH 30,
        text  TYPE c LENGTH 40,
        sort1 TYPE c LENGTH 20,
      END OF ts_equi.
    TYPES tt_equi TYPE STANDARD TABLE OF ts_equi WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_op,
        project_id  TYPE c LENGTH 24,
        id          TYPE c LENGTH 32,
        bukrs       TYPE c LENGTH 4,
        text        TYPE c LENGTH 40,
        long        TYPE c LENGTH 132,
        phase_op    TYPE c LENGTH 31,
        type        TYPE c LENGTH 15,
        unit_ge     TYPE n LENGTH 8,
        unit_te     TYPE n LENGTH 8,
        unit_ep     TYPE n LENGTH 8,
        prio_dg     TYPE c LENGTH 40,
        prio_dt     TYPE c LENGTH 40,
        prio_ma     TYPE c LENGTH 40,
        strat_axis  TYPE c LENGTH 8,
        fonc_axis   TYPE c LENGTH 8,
        tech_axis   TYPE c LENGTH 8,
        estim_m0    TYPE p LENGTH 6 DECIMALS 2,
        date_m0     TYPE c LENGTH 14,
        fec         TYPE p LENGTH 6 DECIMALS 2,
        date_beg_op TYPE c LENGTH 10,
        date_end_op TYPE c LENGTH 10,
        date_beg_tx TYPE c LENGTH 10,
        date_end_tx TYPE c LENGTH 10,
        equi        TYPE tt_equi,
        prps        TYPE tt_prps,
      END OF ts_op.
    TYPES tt_op TYPE STANDARD TABLE OF ts_op WITH EMPTY KEY.

    DATA gs_op TYPE ts_op.

    "! Return the character representation of IV_FIELD with maximum value either ZZZZZZ for text fields,
    "! 9999.99 for numeric fields, 99991231 for date fields, 235959 for time fields
    "! or FFFFFF for byte fields, depending on the exact length of IV_FIELD.
    METHODS fill IMPORTING iv_field         TYPE any
                 RETURNING VALUE(rv_string) TYPE string.

    METHODS get_op IMPORTING iv_number_of_op         TYPE i
                 RETURNING VALUE(result) TYPE tt_op.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD fill.
    DATA l_ref TYPE REF TO data.

    l_ref ?= cl_abap_exceptional_values=>get_max_value( in = iv_field ).
    ASSIGN l_ref->* TO FIELD-SYMBOL(<value>).
    rv_string = |{ <value> }|.
    CASE cl_abap_typedescr=>describe_by_data( iv_field )->type_kind.
      WHEN 'C' OR 'g'.
        REPLACE ALL OCCURRENCES OF rv_string(1) IN rv_string WITH 'Z'.
    ENDCASE.
  ENDMETHOD.

  METHOD get_op.
    result = value #( ( gs_op ) ).
    while lines( result ) * 2 <= iv_number_of_op.
      INSERT LINES OF result INTO TABLE result.
    ENDWHILE.
    IF lines( result ) < iv_number_of_op.
      data(lv_missing_number_of_op) = iv_number_of_op - lines( result ).
      INSERT LINES OF result FROM 1 TO lv_missing_number_of_op INTO TABLE result.
    ENDIF.
  ENDMETHOD.

  METHOD setup.
    gs_op = VALUE ts_op(
                        project_id  = fill( VALUE ts_op-project_id( ) )
                         id          = fill( VALUE ts_op-id( ) )
                         bukrs       = fill( VALUE ts_op-bukrs( ) )
                         text        = fill( VALUE ts_op-text( ) )
                         long        = fill( VALUE ts_op-long( ) )
                         phase_op    = fill( VALUE ts_op-phase_op( ) )
                         type        = fill( VALUE ts_op-type( ) )
                         unit_ge     = fill( VALUE ts_op-unit_ge( ) )
                         unit_te     = fill( VALUE ts_op-unit_te( ) )
                         unit_ep     = fill( VALUE ts_op-unit_ep( ) )
                         prio_dg     = fill( VALUE ts_op-prio_dg( ) )
                         prio_dt     = fill( VALUE ts_op-prio_dt( ) )
                         prio_ma     = fill( VALUE ts_op-prio_ma( ) )
                         strat_axis  = fill( VALUE ts_op-strat_axis( ) )
                         fonc_axis   = fill( VALUE ts_op-fonc_axis( ) )
                         tech_axis   = fill( VALUE ts_op-tech_axis( ) )
                         estim_m0    = fill( VALUE ts_op-estim_m0( ) )
                         date_m0     = sy-datum
                         fec         = fill( VALUE ts_op-fec( ) )
                         date_beg_op = sy-datum
                         date_end_op = sy-datum
                         date_beg_tx = sy-datum
                         date_end_tx = sy-datum
                         equi        = VALUE #( ( main  = fill( VALUE ts_equi-main( ) )
                                                  tplnr = fill( VALUE ts_equi-tplnr( ) )
                                                  text  = fill( VALUE ts_equi-text( ) )
                                                  sort1 = fill( VALUE ts_equi-sort1( ) ) ) )
                         prps        = VALUE #( ( posid         = fill( VALUE ts_prps-posid( ) )
                                                  text          = fill( VALUE ts_prps-text( ) )
                                                  prog_year     = fill( VALUE ts_prps-prog_year( ) )
                                                  prart         = fill( VALUE ts_prps-prart( ) )
                                                  bdg_type      = fill( VALUE ts_prps-bdg_type( ) )
                                                  dir           = fill( VALUE ts_prps-dir( ) )
                                                  section       = fill( VALUE ts_prps-section( ) )
                                                  fincode_num   = fill( VALUE ts_prps-fincode_num( ) )
                                                  fincode_short = fill( VALUE ts_prps-fincode_short( ) )
                                                  fincode_long  = fill( VALUE ts_prps-fincode_long( ) )
                                                  bdgserv_num   = fill( VALUE ts_prps-bdgserv_num( ) )
                                                  bdgserv_text  = fill( VALUE ts_prps-bdgserv_text( ) )
                                                  fistl_ge      = fill( VALUE ts_prps-fistl_ge( ) )
                                                  fistl_te      = fill( VALUE ts_prps-fistl_te( ) )
                                                  kostl         = fill( VALUE ts_prps-kostl( ) )
                                                  amount_d      = fill( VALUE ts_prps-amount_d( ) )
                                                  amount_v      = fill( VALUE ts_prps-amount_v( ) )
                                                  amount_p      = fill( VALUE ts_prps-amount_p( ) )
                                                  status        = fill( VALUE ts_prps-status( ) )
                                                  year          = VALUE #( ( year     = fill( VALUE ts_year-year( ) )
                                                                             amount_c = fill( VALUE ts_year-amount_c( ) )
                                                                             amount_j = fill( VALUE ts_year-amount_j( ) )
                                                                             amount   = fill( VALUE ts_year-amount( ) ) ) ) ) ) ).
  ENDMETHOD.

  METHOD st.
    " No memory error with 1152000.
    " Memory error starting with 1154000.
    WAIT UP TO 3 SECONDS.
    DATA(lt_op) = get_op( 1140000 ).
    CALL TRANSFORMATION zzsro_xslt_memory_versus_st
         SOURCE data = lt_op
         RESULT XML DATA(lv_xml_xstring) ##NEEDED.
    " message x001(00).
  ENDMETHOD.

  METHOD xslt.
    " No memory error with 172000.
    " Memory error starting with 175000.
    WAIT UP TO 3 SECONDS.
    DATA(lt_op) = get_op( 160000 ).
    CALL TRANSFORMATION zzsro_xslt_memory
         SOURCE data = lt_op
         RESULT XML DATA(lv_xml_xstring) ##NEEDED.
    " message x001(00).
  ENDMETHOD.
ENDCLASS.
