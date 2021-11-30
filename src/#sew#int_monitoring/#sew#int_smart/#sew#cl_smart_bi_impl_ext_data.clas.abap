class /SEW/CL_SMART_BI_IMPL_EXT_DATA definition
  public
  final
  create public .

public section.

  interfaces /MHP/SMART_IF_REPO_EXT_HDLR .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_SMART_BI_IMPL_EXT_DATA IMPLEMENTATION.


  method /MHP/SMART_IF_REPO_EXT_HDLR~EXTEND_OBJECTSELECTION.
**********************************************************************
* Check object selection based on filter strings
**********************************************************************

* first of all check if we are in entityset query
    check io_entityset_context is bound and io_entityset_context->get_filter( )->get_filter_string( ) is not initial.

* read sel options
    data(lv_filter_string) = io_entityset_context->get_filter( )->get_filter_string( ).
    data(lt_sel_opt_filter) = value string_t( ( |YEAR| ) ( |PERIOD| ) ( |QUARTER| ) ).

    /mhp/smart_cl_gw_helper=>create_wherestmt_selopt( exporting iv_wherestring = lv_filter_string
      it_selectoption_filter = lt_sel_opt_filter importing et_select_options = data(lt_select_options) ).

    data:
      begin of ls_datesel,
        begda type begda,
        endda type endda,
      end of ls_datesel.

* check if we have any filter attributes containing year field
    if line_exists( lt_select_options[ property = 'YEAR' ] ).
      loop at lt_select_options[ property = 'YEAR' ]-select_options into data(ls_select_option_year) where option = 'EQ'.
        data(lv_year) = conv char4( ls_select_option_year-low ).
        if ls_datesel-begda(4) gt lv_year or ls_datesel-begda is initial.
          ls_datesel-begda = |{ lv_year }0101|.
        endif.
        if ls_datesel-endda(4) lt lv_year or ls_datesel-endda is initial.
          ls_datesel-endda = |{ lv_year }1231|.
        endif.
      endloop.
    endif.

* check if we have any filter attributes containing period field
    if line_exists( lt_select_options[ property = 'PERIOD' ] ).
      data(lt_selopt_per) = lt_select_options[ property = 'PERIOD' ]-select_options.
      delete lt_selopt_per where option ne 'EQ'.
      if lines( lt_selopt_per ) > 0.
        sort lt_selopt_per by low.
* let's pic the first and the last one and set begda and endda
        ls_datesel-begda = |{ lt_selopt_per[ 1 ]-low }01|.

        ls_datesel-endda = |{ lt_selopt_per[ lines( lt_selopt_per ) ]-low }01|.
        ls_datesel-endda = cl_reca_date=>set_to_end_of_month( ls_datesel-endda ).
      endif.
    endif.

* at least check quarter parameter
    if line_exists( lt_select_options[ property = 'QUARTER' ] ).
      data(lt_selopt_qua) = lt_select_options[ property = 'QUARTER' ]-select_options.
      delete lt_selopt_qua where option ne 'EQ'.
      if lines( lt_selopt_qua ) > 0.
        sort lt_selopt_qua by low.

        if ls_datesel-begda is initial.
          ls_datesel-begda = cs_objselect-objselect_begda.
        endif.
        data(lv_quarter_month_low) = switch #( lt_selopt_qua[ 1 ]-low when 'Q1' then '01' when  'Q2' then '04' when  'Q3' then '07' else '10' ).
        ls_datesel-begda+4(4) = |{ lv_quarter_month_low }01|.

        if ls_datesel-endda is initial.
          ls_datesel-endda = cs_objselect-objselect_endda.
        endif.
        data(lv_quarter_month_high) =  switch #( lt_selopt_qua[ lines( lt_selopt_qua ) ]-low when 'Q1' then '03' when  'Q2' then '06' when  'Q3' then '09' else '12' ).
        ls_datesel-endda+4(4) = |{ lv_quarter_month_high }01|.
        ls_datesel-endda = cl_reca_date=>set_to_end_of_month( ls_datesel-endda ).
      endif.
    endif.

    if ls_datesel is not initial.
      cs_objselect-objselect_begda = ls_datesel-begda.
      cs_objselect-objselect_endda = ls_datesel-endda.
    endif.
  endmethod.


  method /MHP/SMART_IF_REPO_EXT_HDLR~EXTEND_STRUCTURE_DEFINITION.
  endmethod.


  method /MHP/SMART_IF_REPO_EXT_HDLR~EXTEND_TABLE_DATA.
  endmethod.
ENDCLASS.
