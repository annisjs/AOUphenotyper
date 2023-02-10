#' Hypertension
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hypertension.csv
#' @details At least 1 BP >= 140/90 or 1 med.
#'
#' Meds: "acebutolol","atenolol","betaxolol","bisoprolol","carvedilol","esmolol","labetalol","metoprolol","nadolol","nebivolol"
#' ,"pindolol","propranolol","sotalol","timolol","acetazolamide","dichlorphenamide","methazolamide","adenosine","amiodarone",
#' "bretylium","dextrose%lidocaine","disopyramide","dofetilide","dronedarone","flecainide","ibutilide","lidocaine","mexiletine",
#' "procainamide","propafenone","quinidine","alfuzosin","doxazosin","prazosin","silodosin","tamsulosin","terazosin","alirocumab",
#' "atorvastatin","bempedoic acid","bempedoic acid%ezetimibe","cholestyramine","colesevelam","colestipol","evinacumab-dgnb","evolocumab",
#' "ezetimibe","ezetimibe%rosuvastatin","ezetimibe%simvastatin","fenofibrate","fenofibric acid","fluvastatin","gemfibrozil",
#' "icosapent ethyl","inv-cirb#20-18 (preventable) atorvastatin 40mg%pbo","lomitapide","lovastatin","omega-3 acid","pitavastatin",
#' "pravastatin","rosuvastatin","simvastatin","aliskiren","aliskiren%hydrochlorothiazide","amlodipine%benazepril",
#' "amlodipine%hydrochlorothiazide%olmesartan","amlodipine%hydrochlorothiazide%valsartan","amlodipine%olmesartan","amlodipine%perindopril",
#' "amlodipine%telmisartan","amlodipine%valsartan","atenolol%chlorthalidone","azilsartan%chlorthalidone","benazepril%hydrochlorothiazide",
#' "bendroflumethiazide%nadolol","bisoprolol%hydrochlorothiazide","candesartan%hydrochlorothiazide","captopril%hydrochlorothiazide",
#' "enalapril%hydrochlorothiazide","eprosartan%hydrochlorothiazide","fosinopril%hydrochlorothiazide","hydralazine%isosorbide",
#' "hydrochlorothiazide%irbesartan","hydrochlorothiazide%lisinopril","hydrochlorothiazide%losartan","hydrochlorothiazide%methyldopa",
#' "hydrochlorothiazide%metoprolol","hydrochlorothiazide%moexipril","hydrochlorothiazide%olmesartan","hydrochlorothiazide%propranolol",
#' "hydrochlorothiazide%quinapril","hydrochlorothiazide%telmisartan","hydrochlorothiazide%valsartan","sacubitril%valsartan",
#' "trandolapril%verapamil","amiloride","amiloride%hydrochlorothiazide","eplerenone","finerenone","hydrochlorothiazide%spironolactone",
#' "hydrochlorothiazide%triamterene","spironolactone","triamterene","amlodipine","amlodipine%atorvastatin","amlodipine%celecoxib",
#' "clevidipine","diltiazem","felodipine","isradipine","nicardipine","nifedipine","nimodipine","nisoldipine","verapamil","amyl nitrite",
#' "isosorbide dinitrate","isosorbide mononitrate","nitroglycerin","ranolazine","azilsartan","candesartan","eprosartan","irbesartan",
#' "losartan","olmesartan","telmisartan","valsartan","benazepril","captopril","enalapril","enalaprilat","fosinopril","lisinopril",
#' "moexipril","perindopril","quinapril","ramipril","trandolapril","bumetanide","ethacrynic acid","furosemide","torsemide","
#' caffeine%magnesium salicylate","mannitol","pamabrom","spironolactone","chlorothiazide","chlorthalidone","hydrochlorothiazide",
#' "indapamide","metolazone","clonidine","guanabenz","guanfacine","hydralazine","iloprost","lofexidine","macitentan","mecamylamine",
#' "methyldopa","methyldopate","metyrosine","minoxidil","riociguat","sildenafil","sodium nitroprusside","tadalafil","ethanolamine oleate",
#' "morrhuate sodium","polidocanol","sodium tetradecyl sulfate","isoxsuprine","papaverine"
#' @import data.table stringr
#' @export
hypertension <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  meds <- c("acebutolol","atenolol","betaxolol","bisoprolol","carvedilol","esmolol","labetalol","metoprolol","nadolol","nebivolol","pindolol","propranolol","sotalol","timolol","acetazolamide","dichlorphenamide","methazolamide","adenosine","amiodarone","bretylium","dextrose%lidocaine","disopyramide","dofetilide","dronedarone","flecainide","ibutilide","lidocaine","mexiletine","procainamide","propafenone","quinidine","alfuzosin","doxazosin","prazosin","silodosin","tamsulosin","terazosin","alirocumab","atorvastatin","bempedoic acid","bempedoic acid%ezetimibe","cholestyramine","colesevelam","colestipol","evinacumab-dgnb","evolocumab","ezetimibe","ezetimibe%rosuvastatin","ezetimibe%simvastatin","fenofibrate","fenofibric acid","fluvastatin","gemfibrozil","icosapent ethyl","inv-cirb#20-18 (preventable) atorvastatin 40mg%pbo","lomitapide","lovastatin","omega-3 acid","pitavastatin","pravastatin","rosuvastatin","simvastatin","aliskiren","aliskiren%hydrochlorothiazide","amlodipine%benazepril","amlodipine%hydrochlorothiazide%olmesartan","amlodipine%hydrochlorothiazide%valsartan","amlodipine%olmesartan","amlodipine%perindopril","amlodipine%telmisartan","amlodipine%valsartan","atenolol%chlorthalidone","azilsartan%chlorthalidone","benazepril%hydrochlorothiazide","bendroflumethiazide%nadolol","bisoprolol%hydrochlorothiazide","candesartan%hydrochlorothiazide","captopril%hydrochlorothiazide","enalapril%hydrochlorothiazide","eprosartan%hydrochlorothiazide","fosinopril%hydrochlorothiazide","hydralazine%isosorbide","hydrochlorothiazide%irbesartan","hydrochlorothiazide%lisinopril","hydrochlorothiazide%losartan","hydrochlorothiazide%methyldopa","hydrochlorothiazide%metoprolol","hydrochlorothiazide%moexipril","hydrochlorothiazide%olmesartan","hydrochlorothiazide%propranolol","hydrochlorothiazide%quinapril","hydrochlorothiazide%telmisartan","hydrochlorothiazide%valsartan","sacubitril%valsartan","trandolapril%verapamil","amiloride","amiloride%hydrochlorothiazide","eplerenone","finerenone","hydrochlorothiazide%spironolactone","hydrochlorothiazide%triamterene","spironolactone","triamterene","amlodipine","amlodipine%atorvastatin","amlodipine%celecoxib","clevidipine","diltiazem","felodipine","isradipine","nicardipine","nifedipine","nimodipine","nisoldipine","verapamil","amyl nitrite","isosorbide dinitrate","isosorbide mononitrate","nitroglycerin","ranolazine","azilsartan","candesartan","eprosartan","irbesartan","losartan","olmesartan","telmisartan","valsartan","benazepril","captopril","enalapril","enalaprilat","fosinopril","lisinopril","moexipril","perindopril","quinapril","ramipril","trandolapril","bumetanide","ethacrynic acid","furosemide","torsemide","caffeine%magnesium salicylate","mannitol","pamabrom","spironolactone","chlorothiazide","chlorthalidone","hydrochlorothiazide","indapamide","metolazone","clonidine","guanabenz","guanfacine","hydralazine","iloprost","lofexidine","macitentan","mecamylamine","methyldopa","methyldopate","metyrosine","minoxidil","riociguat","sildenafil","sodium nitroprusside","tadalafil","ethanolamine oleate","morrhuate sodium","polidocanol","sodium tetradecyl sulfate","isoxsuprine","papaverine")
  bp_meds <- med_query(dataset,meds)
  high_bp <- high_bp_query(dataset)
  if (!is.null(anchor_date_table))
  {
    bp_meds <- as.data.table(merge(bp_meds,anchor_date_table,by="person_id"))
    bp_meds[,min_window_date := anchor_date + before]
    bp_meds[,max_window_date := anchor_date + after]
    bp_meds <- bp_meds[drug_exposure_start_date >= min_window_date]
    bp_meds <- bp_meds[drug_exposure_start_date <= max_window_date]

    high_bp <- as.data.table(merge(high_bp,anchor_date_table,by="person_id"))
    high_bp[,min_window_date := anchor_date + before]
    high_bp[,max_window_date := anchor_date + after]
    high_bp <- high_bp[measurement_date >= min_window_date]
    high_bp <- high_bp[measurement_date <= max_window_date]
  }
  result_med_agg <- setDT(bp_meds)[,.(med_status = length(drug_exposure_start_date) > 0,
                                      med_entry_date = min(drug_exposure_start_date)),.(person_id)]
  result_bp_agg <- setDT(high_bp)[,.(bp_status = length(measurement_date) > 0,
                                     bp_entry_date = min(measurement_date)),.(person_id)]
  result_all <- merge(result_med_agg,result_bp_agg,by="person_id",all.x=TRUE,all.y=TRUE)
  result_all$bp_status[is.na(result_all$bp_status)] <- FALSE
  result_all$med_status[is.na(result_all$med_status)] <- FALSE
  result_all <- result_all[,.(hypertension_entry_date = min(c(med_entry_date,bp_entry_date),na.rm=T),
                              hypertension_status = med_status | bp_status),.(person_id)]
  fwrite(result_all,file="hypertension.csv")
  system(str_glue("gsutil cp hypertension.csv {output_folder}/hypertension.csv"),intern=TRUE)
}
