!Common Community Physics Package (CCPP) wrapper for PUMAS.
module micro_pumas_ccpp

  implicit none
  private
  save

  public :: micro_pumas_ccpp_init
  public :: micro_pumas_ccpp_run

contains

  !> \section arg_table_micro_pumas_ccpp_init Argument Table
  !! \htmlinclude micro_pumas_ccpp_init.html
  subroutine micro_pumas_ccpp_init(gravit, rair, rh2o, cpair, tmelt, latvap, latice,    &
                                   rhmini, iulog, micro_mg_do_hail, micro_mg_do_graupel, &
                                   microp_uniform, do_cldice, use_hetfrz_classnuc,      &
                                   remove_supersat, micro_mg_evap_sed_off,              &
                                   micro_mg_icenuc_rh_off, mmicro_mg_icenuc_use_meyers, &
                                   micro_mg_evap_scl_ifs, micro_mg_evap_rhthrsh_ifs,    &
                                   micro_mg_rainfreeze_ifs, micro_mg_ifs_sed,           &
                                   micro_mg_precip_fall_corr, micro_mg_accre_sees_auto, &
                                   micro_mg_implicit_fall, micro_mg_nccons,             &
                                   micro_mg_nicons, micro_mg_ngcons, micro_mg_nrcons,   &
                                   micro_mg_nscons, micro_mg_precip_frac_method,        &
                                   micro_mg_warm_rain,
                                   stochastic_emulated_filename_quantile,               &
                                   stochastic_emulated_filename_input_scale,            &
                                   stochastic_emulated_filename_output_scale,           &
                                   micro_mg_dcs_in, &
                                   micro_mg_berg_eff_factor_in, micro_mg_accre_enhan_fact_in, &
                                   micro_mg_autocon_fact_in, micro_mg_autocon_nd_exp_in,      &
                                   micro_mg_autocon_lwp_exp_in, micro_mg_homog_size_in,       &
                                   micro_mg_vtrmi_factor_in,    micro_mg_vtrms_factor_in,     &
                                   micro_mg_effi_factor_in,     micro_mg_iaccr_factor_in,     &
                                   micro_mg_max_nicons_in, micro_mg_ncnst_in,                 &
                                   micro_mg_ninst_in, micro_mg_ngnst_in, micro_mg_nrnst_in,   &
                                   micro_mg_nsnst_in, errmsg, errcode)

  !External dependencies:
  use ccpp_kinds,       only: kind_phys
  use micro_pumas_init, only: micro_pumas_init
  use pumas_kinds,      only: pumas_r8=>kind_r8

  !Subroutine (dummy) arguments:

  !Host model constants:
  real(kind_phys), intent(in) :: gravit !standard gravitational acceleration                    (m s-2)
  real(kind_phys), intent(in) :: rair   !gas constant for dry air                               (J kg-1 K-1)
  real(kind_phys), intent(in) :: rh2o   !gas constat for water vapor                            (J kg-1 K-1)
  real(kind_phys), intent(in) :: cpair  !specific heat of dry air at constant pressure          (J kg-1 K-1)
  real(kind_phys), intent(in) :: tmelt  !freezing point of water                                (K)
  real(kind_phys), intent(in) :: latvap !latent heat of vaporization of water at 0 degrees C    (J kg-1)
  real(kind_phys), intent(in) :: latice !latent heat of fusion of water at 0 degrees C          (J kg-1)
  real(kind_phys), intent(in) :: rhmini !Minimum RH for ice cloud fraction > 0                  (fraction)

  !Host model variables:
  integer, intent(in) :: iulog          !Log output unit number (1)

  !PUMAS-specific parameters:
  !-------------------------
  logical, intent(in) :: micro_mg_do_hail           !flag for PUMAS to simulate hail                              (flag)
  logical, intent(in) :: micro_mg_do_graupel        !flag for PUMAS to simulate graupel                           (flag)
  logical, intent(in) :: microp_uniform             !flag for PUMAS to perform uniform calc.                      (flag)
  logical, intent(in) :: do_cldice                  !flag for PUMAS to simulate cloud ice                         (flag)
  logical, intent(in) :: use_hetfrz_classnuc        !flag to turn on PUMAS heterogeneous freezing                 (flag)
  logical, intent(in) :: remove_supersat            !flag to remove supersaturation after sedimentation loop      (flag)
  logical, intent(in) :: micro_mg_evap_sed_off      !flag to turn off condensate evap. after sedimentation        (flag)
  logical, intent(in) :: micro_mg_icenuc_rh_off     !flag to turn off RH threshold for ice nucleation             (flag)
  logical, intent(in) :: micro_mg_icenuc_use_meyers !flag to use Meyers 1992 temp. dependent ice nucleation       (flag)
  logical, intent(in) :: micro_mg_evap_scl_ifs      !flag to apply IFS precipitation evap. scaling                (flag)
  logical, intent(in) :: micro_mg_evap_rhthrsh_ifs  !flag to use IFS precipitation evap. RH threshold             (flag)
  logical, intent(in) :: micro_mg_rainfreeze_ifs    !flag to freeze rain at 0 degrees C as is done in IFS         (flag)
  logical, intent(in) :: micro_mg_ifs_sed           !flag to use IFS sedimentation fall speeds                    (flag)
  logical, intent(in) :: micro_mg_precip_fall_corr  !flag to ensure non-zero precip fall speed if precip above    (flag)
  logical, intent(in) :: micro_mg_accre_sees_auto   !flag to add autoconverted liuqid to rain before accretion    (flag)
  logical, intent(in) :: micro_mg_implicit_fall     !flag to use implicit fall speed routine for all hydrometeors (flag)
  logical, intent(in) :: micro_mg_nccons            !flag to have PUMAS hold cloud droplet number constant        (flag)
  logical, intent(in) :: micro_mg_nicons            !flag to have PUMAS hold cloud ice number constant            (flag)
  logical, intent(in) :: micro_mg_ngcons            !flag to have PUMAS hold cloud graupel number constant        (flag)
  logical, intent(in) :: micro_mg_nrcons            !flag to have PUMAS hold cloud rain number constant           (flag)
  logical, intent(in) :: micro_mg_nscons            !flag to have PUMAS hold cloud snow number constant           (flag)

  !type of precipitation fraction method (none):
  character(len=*), intent(in) :: micro_mg_precip_frac_method
  !type of warm rain autoconversion/accr.method to use (none):
  character(len=*), intent(in) :: micro_mg_warm_rain
  !neural net file for warm_rain machine learning (none):
  character(len=*), intent(in) :: stochastic_emulated_filename_quantile
  !neural net input scaling values files for warm_rain machine learning (none):
  character(len=*), intent(in) :: stochastic_emulated_filename_input_scale
  !Neural net output scaling values file for warm_rain machine learning (none):
  character(len=*), intent(in) :: stochastic_emulated_filename_output_scale

  real(kind_phys), intent(in) :: micro_mg_dcs_in              !autoconversion size threshold                      (um)
  real(kind_phys), intent(in) :: micro_mg_berg_eff_factor_in  !efficienty factor for Bergeron process             (1)
  real(kind_phys), intent(in) :: micro_mg_accre_enhan_fact_in !accretion enhancement factor                       (1)
  real(kind_phys), intent(in) :: micro_mg_autocon_fact_in     !autoconverion enhancement prefactor                (1)
  real(kind_phys), intent(in) :: micro_mg_autocon_nd_exp_in   !autconversion cloud liquid exponent factor         (1)
  real(kind_phys), intent(in) :: micro_mg_autocon_lwp_exp_in  !autoconversion LWP exponent factor                 (1)
  real(kind_phys), intent(in) :: micro_mg_homog_size_in       !mean volume radius of homoegenous freezing ice     (m)
  real(kind_phys), intent(in) :: micro_mg_vtrmi_factor_in     !ice fall velocity enhancement factor               (1)
  real(kind_phys), intent(in) :: micro_mg_vtrms_factor_in     !snow fall velocity enhancement factor              (1)
  real(kind_phys), intent(in) :: micro_mg_effi_factor_in      !ice effective radius enhancement factor            (1)
  real(kind_phys), intent(in) :: micro_mg_iaccr_factor_in     !ice accretion factor                               (1)
  real(kind_phys), intent(in) :: micro_mg_max_nicons_in       !max allowed ice number concentration               (m-3)

  !In-cloud droplet number concentration if micro_mg_nccons is True (m-3):
  real(kind_phys), intent(in) :: micro_mg_ncnst_in
  !In-cloud ice number concentration if micro_mg_nicons is True     (m-3):
  real(kind_phys), intent(in) :: micro_mg_ninst_in
  !In-cloud graupel number concentration if micro_mg_ngcons is True (m-3):
  real(kind_phys), intent(in) :: micro_mg_ngnst_in
  !In-cloud rain number concentration when micro_mg_nrcons is True  (m-3):
  real(kind_phys), intent(in) :: micro_mg_nrnst_in
  !In-cloud snow number concentration when micro_mg_nscons is True  (m-3):
  real(kind_phys), intent(in) :: micro_mg_nsnst_in
  !-------------------------

  !Output variables:
  character(len=512), intent(out) :: errmsg  !PUMAS/CCPP error message (none)
  integer,            intent(out) :: errcode !CCPP error code (1)

  !Local variables:
  real(pumas_r8), intent(in) :: micro_mg_dcs              !autoconversion size threshold                      (um)
  real(pumas_r8), intent(in) :: micro_mg_berg_eff_factor  !efficienty factor for Bergeron process             (1)
  real(pumas_r8), intent(in) :: micro_mg_accre_enhan_fact !accretion enhancement factor                       (1)
  real(pumas_r8), intent(in) :: micro_mg_autocon_fact     !autoconverion enhancement prefactor                (1)
  real(pumas_r8), intent(in) :: micro_mg_autocon_nd_exp   !autconversion cloud liquid exponent factor         (1)
  real(pumas_r8), intent(in) :: micro_mg_autocon_lwp_exp  !autoconversion LWP exponent factor                 (1)
  real(pumas_r8), intent(in) :: micro_mg_homog_size       !mean volume radius of homoegenous freezing ice     (m)
  real(pumas_r8), intent(in) :: micro_mg_vtrmi_factor     !ice fall velocity enhancement factor               (1)
  real(pumas_r8), intent(in) :: micro_mg_vtrms_factor     !snow fall velocity enhancement factor              (1)
  real(pumas_r8), intent(in) :: micro_mg_effi_factor      !ice effective radius enhancement factor            (1)
  real(pumas_r8), intent(in) :: micro_mg_iaccr_factor     !ice accretion factor                               (1)
  real(pumas_r8), intent(in) :: micro_mg_max_nicons       !max allowed ice number concentration               (m-3)

  !In-cloud droplet number concentration if micro_mg_nccons is True (m-3):
  real(pumas_r8), intent(in) :: micro_mg_ncnst
  !In-cloud ice number concentration if micro_mg_nicons is True     (m-3):
  real(pumas_r8), intent(in) :: micro_mg_ninst
  !In-cloud graupel number concentration if micro_mg_ngcons is True (m-3):
  real(pumas_r8), intent(in) :: micro_mg_ngnst
  !In-cloud rain number concentration when micro_mg_nrcons is True  (m-3):
  real(pumas_r8), intent(in) :: micro_mg_nrnst
  !In-cloud snow number concentration when micro_mg_nscons is True  (m-3):
  real(pumas_r8), intent(in) :: micro_mg_nsnst

  !Initialize error code:
  errcode = 0

  !Convert real-type input fields into appropriate kind:
  micro_mg_dcs              = real(micro_mg_dcs_in, pumas_r8)
  micro_mg_berg_eff_factor  = real(micro_mg_berg_eff_factor_in, pumas_r8)
  micro_mg_accre_enhan_fact = real(micro_mg_accre_enhan_fact_in, pumas_r8)
  micro_mg_autocon_fact     = real(micro_mg_autocon_fact_in, pumas_r8)
  micro_mg_autocon_nd_exp   = real(micro_mg_autocon_nd_exp_in, pumas_r8)
  micro_mg_autocon_lwp_exp  = real(micro_mg_autocon_lwp_exp_in, pumas_r8)
  micro_mg_homog_size       = real(micro_mg_homog_size_in, pumas_r8)
  micro_mg_vtrmi_factor     = real(micro_mg_vtrmi_factor_in, pumas_r8)
  micro_mg_vtrms_factor     = real(micro_mg_vtrms_factor_in, pumas_r8)
  micro_mg_effi_factor      = real(micro_mg_effi_factor_in, pumas_r8)
  micro_mg_iaccr_factor     = real(micro_mg_iaccr_factor_in, pumas_r8)
  micro_mg_max_nicons       = real(micro_mg_max_nicons_in, pumas_r8)

  !Call PUMAS initialization routine:
  call micro_pumas_init( &
           pumas_r8, gravit, rair, rh2o, cpair, &
           tmelt, latvap, latice, rhmini, &
           micro_mg_dcs,                  &
           micro_mg_do_hail,micro_mg_do_graupel, &
           microp_uniform, do_cldice, use_hetfrz_classnuc, &
           micro_mg_precip_frac_method, micro_mg_berg_eff_factor, &
           micro_mg_accre_enhan_fact , &
           micro_mg_autocon_fact , micro_mg_autocon_nd_exp, micro_mg_autocon_lwp_exp, micro_mg_homog_size, &
           micro_mg_vtrmi_factor, micro_mg_vtrms_factor, micro_mg_effi_factor, &
           micro_mg_iaccr_factor, micro_mg_max_nicons, &
           remove_supersat, micro_mg_warm_rain, &
           micro_mg_evap_sed_off, micro_mg_icenuc_rh_off, micro_mg_icenuc_use_meyers, &
           micro_mg_evap_scl_ifs, micro_mg_evap_rhthrsh_ifs, &
           micro_mg_rainfreeze_ifs,  micro_mg_ifs_sed, micro_mg_precip_fall_corr,&
           micro_mg_accre_sees_auto, micro_mg_implicit_fall, &
           micro_mg_nccons, micro_mg_nicons, micro_mg_ncnst, &
           micro_mg_ninst, micro_mg_ngcons, micro_mg_ngnst, &
           micro_mg_nrcons, micro_mg_nrnst, micro_mg_nscons, micro_mg_nsnst, &
           stochastic_emulated_filename_quantile, stochastic_emulated_filename_input_scale, &
           stochastic_emulated_filename_output_scale, iulog, errmsg)

  !Set error code to non-zero value if PUMAS returns an error message:
  if (trim(errmsg) /= "") then
    errcode = 1
  end if

  end subroutine micro_pumas_ccpp_init

  !> \section arg_table_micro_pumas_ccpp_run Argument Table
  !! \htmlinclude micro_pumas_ccpp_run.html
  subroutine micro_pumas_ccpp_run(micro_ncol, micro_nlev, micro_timestep,           &
                                  micro_airT_in, micro_airq_in, micro_cldliq_in,    &
                                  micro_cldice_in, micro_numliq_in,                 &
                                  micro_numeice_in, micro_rainliq_in,               &
                                  micro_snowice_in, micro_numrain_in,               &
                                  micro_graupice_in, micro_numgraup_in,             &
                                  micro_numsnow_in, micro_relvar_in,                &
                                  micro_accre_enhan_in, micro_pmid_in,              &
                                  micro_pdel_in, micro_pint_in,                     &
                                  micro_strat_cldfrc_in, micro_strat_liq_cldfrc_in, &
                                  micro_strat_ice_cldfrc_in, micro_qsatfac_in,      &
                                  micro_naai_in, micro_npccn_in,                    &
                                  micro_rndst_in, micro_nacon_in,                   &
                                  micro_qcsinksum_rate1ord_out, &
                                  errmsg, errcode)

    !Subroutine (dummy) input arguments:

    !Host model dimensions/parameters:
    integer,         intent(in) :: micro_ncol         !Number of horizontal microphysics columns (count)
    integer,         intent(in) :: micro_nlev         !Number of microphysics vertical layers (count)
    real(kind_phys), intent(in) :: micro_timestep     !Microphysics time step (s)

    !Host model state variables:

    !Microphysics Air temperature (K) of new state
    real(kind_phys), intent(in) :: micro_airT_in(micro_ncol, micro_nlev)
    !Microphysics Water vapor mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(kind_phys), intent(in) :: micro_airq_in(micro_ncol, micro_nlev)
    !Microphysics cloud liquid water mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(kind_phys), intent(in) :: micro_cldliq_in(micro_ncol, micro_nlev)
    !Microphysics cloud ice mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(kind_phys), intent(in) :: micro_cldice_in(micro_ncol, micro_nlev)
    !microphysics mass number concentration of cloud liquid water wrt moist air and condensed water of new state (kg-1)
    real(kind_phys), intent(in) :: micro_numliq_in(micro_ncol, micro_nlev)
    !microphysics mass number concentration of cloud ice wrt moist air and condensed water of new state (kg-1)
    real(kind_phys), intent(in) :: micro_numice_in(micro_ncol, micro_nlev)
    !microphysics rain mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(kind_phys), intent(in) :: micro_rainliq_in(micro_ncol, micro_nlev)
    !microphysics snow mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(kind_phys), intent(in) :: micro_snowice_in(micro_ncol, micro_nlev)
    !microphysics mass number concentration of rain wrt moist air and condensed water of new state (kg-1)
    real(kind_phys), intent(in) :: micro_numrain_in(micro_ncol, micro_nlev)
    !microphysics mass number concentration of snow wrt moist air and condensed water of new state (kg-1)
    real(kind_phys), intent(in) :: micro_numsnow_in(micro_ncol, micro_nlev)
    !microphysics graupel mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(kind_phys), intent(in) :: micro_graupice_in(micro_ncol, micro_nlev)
    !microphysics mass number concentration of graupel wrt moist air and condensed water of new state (kg-1)
    real(kind_phys), intent(in) :: micro_numgraup_in(micro_ncol, micro_nlev)
    !microphysics relative variance of cloud water (1)
    real(kind_phys), intent(in) :: micro_relvar_in(micro_ncol, micro_nlev)
    !microphysics accretion enhancement factor (1)
    real(kind_phys), intent(in) :: micro_accre_enhan_in(micro_ncol, micro_nlev)
    !microphysics air pressure (Pa)
    real(kind_phys), intent(in) :: micro_pmid_in(micro_ncol, micro_nlev)
    !microphysics air pressure thickness (Pa)
    real(kind_phys), intent(in) :: micro_pdel_in(micro_ncol, micro_nlev)
    !microphysics air pressure at interfaces (Pa)
    real(kind_phys), intent(in) :: micro_pint_in(micro_ncol, micro_nlev)
    !microphysics stratiform cloud area fraction (fraction)
    real(kind_phys), intent(in) :: micro_strat_cldfrc_in(micro_ncol, micro_nlev)
    !microphysics stratiform cloud liquid area fraction (fraction)
    real(kind_phys), intent(in) :: micro_strat_liq_cldfrc_in(micro_ncol, micro_nlev)
    !microphysics stratiform cloud ice area fraction (fraction)
    real(kind_phys), intent(in) :: micro_strat_ice_cldfrc_in(micro_ncol, micro_nlev)
    !microphysics subgrid cloud water saturation scaling factor (1)
    real(kind_phys), intent(in) :: micro_qsatfac_in(micro_ncol, micro_nlev)
    !microphysics tendency of activated ice nuclei mass number concentration (kg-1 s-1)
    real(kind_phys), intent(in) :: micro_naai_in(micro_ncol, micro_nlev)
    !microphysics tendency of activated cloud condensation nuclei mass number concentration (kg-1 s-1)
    real(kind_phys), intent(in) :: micro_npccn_in(micro_ncol, micro_nlev)
    !microphysics dust radii by size bin  (m)
    real(kind_phys), intent(in) :: micro_rndst_in(micro_ncol, micro_nlev, :)
    !microphysics dust number concentration by size bin (m-3)
    real(kind_phys), intent(in) :: micro_nacon_in(micro_ncol, micro_nlev, :)

    !Subroutine output arguments:

    !microphysics direct conversion rate of stratiform cloud water to precipitation (s-1)
    real(kind_phys), intent(out) :: micro_qcsinksum_rate1ord_out(micro_ncol, micro_nlev)

    character(len=512), intent(out) :: errmsg  !PUMAS/CCPP error message (none)
    integer,            intent(out) :: errcode !CCPP error code (1)

    !Initialize error code:
    errcode = 0

    !Call main PUMAS run routine:
    subroutine micro_pumas_tend ( &
     micro_ncol,             micro_nlev,     micro_timestep,  &
     airT,                   airq,                            &
     cldliq,                 cldice,                          &
     numliq,                 numice,                          &
     rainliq,                snowice,                         &
     numrain,                numsnow,                         &
     graupice,               numgraup,                        &
     relvar,                 accre_enhan,                     &
     pmid,                   pdel, pint,                      &
     strat_cldfrc,           strat_liq_cldfrc,                &
     strat_ice_cldfrc,       qsatfac,                         &
     qcsinksum_rate1ord,                                         &
     naai,                         npccn,                        &
     rndst,                        nacon,                        &
     !CONTINUE HERE!!!!!!!!
     tlat,                         qvlat,                        &
     qctend,                       qitend,                       &
     nctend,                       nitend,                       &
     qrtend,                       qstend,                       &
     nrtend,                       nstend,                       &
     qgtend,                       ngtend,                       &
     effc,               effc_fn,            effi,               &
     sadice,                       sadsnow,                      &
     prect,                        preci,                        &
     nevapr,                       am_evp_st,                    &
     prain,                                                      &
     cmeout,                       deffi,                        &
     pgamrad,                      lamcrad,                      &
     qsout,                        dsout,                        &
     qgout,     ngout,             dgout,                        &
     lflx,               iflx,                                   &
     gflx,                                                       &
     rflx,               sflx,               qrout,              &
     reff_rain,          reff_snow,          reff_grau,          &
     nrout,                        nsout,                        &
     refl,               arefl,              areflz,             &
     frefl,              csrfl,              acsrfl,             &
     fcsrfl,        refl10cm, reflz10cm,     rercld,             &
     ncai,                         ncal,                         &
     qrout2,                       qsout2,                       &
     nrout2,                       nsout2,                       &
     drout2,                       dsout2,                       &
     qgout2,        ngout2,        dgout2,    freqg,                   &
     freqs,                        freqr,                        &
     nfice,                        qcrat,                        &
     proc_rates,                                                 &
     errmsg, & ! Below arguments are "optional" (pass null pointers to omit).
     tnd_qsnow,          tnd_nsnow,          re_ice,             &
     prer_evap,                                                      &
     frzimm,             frzcnt,             frzdep)

  end subroutine micro_pumas_ccpp_run

end module micro_pumas_ccpp
