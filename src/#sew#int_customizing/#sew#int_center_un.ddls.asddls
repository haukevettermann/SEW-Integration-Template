@AbapCatalog.sqlViewName: '/SEW/INT_V_CUN'
@AbapCatalog.preserveKey: true
@AbapCatalog.buffering.status: #ACTIVE
@AbapCatalog.buffering.type: #FULL
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ClientHandling.type: #CLIENT_DEPENDENT
@VDM.viewType: #CONSUMPTION
@EndUserText.label: 'SEW Integration Center PA/OM Union'
define view /SEW/INT_CENTER_UN as 
  select from /sew/int_it_aend as aend
    left outer to one join /sew/int_msg_p as p on  aend.int_run   = p.int_run
                                               and aend.pernr     = p.sap_id
                                               and aend.cloud_id  = p.cloud_id 
                                               and ( 
                                                    aend.aend_id   = p.aend_id 
                                                 or p.aend_id      = ''
                                               ) {
    key aend.mandt,
    key aend.aend_id,
    aend.int_run,
    timestamp,
    cast( left( 
      cast( timestamp as abap.char(32) )
    , 8 ) as abap.dats )                        as run_date,
    cast( pernr as HROBJID )                    as sap_id,
    cast( aend.cloud_pernr as /SEW/DD_ELEMENT ) as cloud_id,
    molga,
    cast( 'P' as otype )                        as otype,
    infty,
    subty,
    objps,
    sprps,
    cast( '' as ISTAT_D)                        as istat,
    cast( '' as priox)                          as priox,
    endda, 
    begda,
    seqnr,
    cast( '' as varyf)                          as varyf,
    operation,
    aend.uname,
    sbgrp_code                                  as sachb_sbgrp,
    sb_code                                     as sachb_code,
    cast( case 
        when sbgrp_code is null and p.user_name is null then '' 
        when sbgrp_code is null then p.user_name
        when sb_code is null then sbgrp_code 
        else concat(sbgrp_code, sb_code)
    end as abap.char(12) )                      as sachb_sbgrpcode,
    p.aedtm                                     as aedtm, 
    status, 
    cast( 1 as abap.int4 )                      as counter_chg,
    cast( case 
        when status = '03' then 1
        else 0
    end as int1 )                               as counter_chg_e,
    cast( case 
        when status = '03' then 1
        when status = '01' then 2
        when status = '05' then 2
        when status = '06' then 2
        when status = '07' then 2
        when status = '02' then 3
        else 0
    end as int1 )                               as criticality_4_status,  
    cast('In' as abap.char(3) )                 as stream_direction
} where aend.aend_id != ''
union
select from /sew/int_it_aeup as aend
    left outer to one join /sew/int_msg_p as p on  aend.int_run   = p.int_run
                                               and aend.pernr     = p.sap_id
                                               and aend.cloud_id  = p.cloud_id  
                                               and ( 
                                                    aend.aend_id   = p.aend_id 
                                                 or p.aend_id      = ''
                                               ) {
    key aend.mandt,
    key aend.aend_id,
    aend.int_run,
    timestamp,
    cast( left( 
      cast( timestamp as abap.char(32) )
    , 8 ) as abap.dats )                        as run_date,
    cast( pernr as HROBJID )                    as sap_id,
    aend.cloud_id,
    molga,
    cast( 'P' as otype )                        as otype,
    infty,
    subty,
    objps,
    sprps,
    cast( '' as ISTAT_D)                        as istat,
    cast( '' as priox)                          as priox,
    endda, 
    begda,
    seqnr,
    cast( '' as varyf)                          as varyf,
    operation,
    aend.uname,
    sbgrp_code                                  as sachb_sbgrp,
    sb_code                                     as sachb_code,
    cast( case 
        when sbgrp_code is null and p.user_name is null then '' 
        when sbgrp_code is null then p.user_name
        when sb_code is null then sbgrp_code 
        else concat(sbgrp_code, sb_code)
    end as abap.char(12) )                      as sachb_sbgrpcode,
    p.aedtm                                     as aedtm, 
    status,
    cast( 1 as abap.int4 )                      as counter_chg,
    cast( case 
        when status = '03' then 1
        else 0
    end as int1 )                               as counter_chg_e,
    cast( case 
        when status = '03' then 1
        when status = '01' then 2
        when status = '05' then 2
        when status = '06' then 2
        when status = '07' then 2
        when status = '02' then 3
        else 0
    end as int1 )                               as criticality_4_status,  
    cast('Out' as abap.char(3) )                as stream_direction
} where aend.aend_id != ''
union
select from /sew/int_om_aend as aend
    left outer to one join /sew/int_msg_p as p on  aend.int_run   = p.int_run
                                               and aend.sap_id    = p.sap_id
                                               and aend.cloud_id  = p.cloud_id 
                                               and ( 
                                                    aend.aend_id   = p.aend_id 
                                                 or p.aend_id      = ''
                                               ) {
    key aend.mandt,
    key aend.aend_id,
    aend.int_run,
    timestamp,
    cast( left( 
      cast( timestamp as abap.char(32) )
    , 8 ) as abap.dats )                        as run_date,
    aend.sap_id,
    aend.cloud_id,
    molga,
    otype,
    infty,
    subty,
    cast( '' as objps)                          as objps,
    cast( '' as sprps)                          as sprps,
    istat,
    priox,
    endda, 
    begda,
    seqnr,
    varyf,
    operation,
    aend.uname,
    sbgrp_code                                  as sachb_sbgrp,
    sb_code                                     as sachb_code,
    cast( case 
        when sbgrp_code is null and p.user_name is null then '' 
        when p.user_name is not null then p.user_name 
        when sb_code is null then sbgrp_code 
        else concat(sbgrp_code, sb_code)
    end as abap.char(12) )                      as sachb_sbgrpcode,
    p.aedtm                                     as aedtm, 
    status,
    cast( 1 as abap.int4 )                      as counter_chg,
    cast( case 
        when status = '03' then 1
        else 0
    end as int1 )                               as counter_chg_e,
    cast( case 
        when status = '03' then 1
        when status = '05' then 1
        when status = '01' then 2
        when status = '07' then 2
        when status = '06' then 2
        when status = '02' then 3
        else 0
    end as int1 )                               as criticality_4_status,  
    cast('In' as abap.char(3) )                 as stream_direction
 } where aend.aend_id != ''
