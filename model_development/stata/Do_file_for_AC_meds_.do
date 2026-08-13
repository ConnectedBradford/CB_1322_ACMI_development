*****************************************************************
* Feb 2021 Code to extract AC medications from BNF/ SNOMED list *
* Include VMP,AMP,VMPP,AMPP (suspect only VMP and AMP needed)   *
* Brand names for each drug included                            *
*****************************************************************
*Import data downloaded from: 
import excel "BNF Snomed Mapping data 20200818.xlsx", sheet("June 2020") firstrow clear

*drop useless variables
drop PresentationPackLevel L M N   

*Make BNF Name lower case
replace BNFName =strlower(BNFName)

*Make new variable that is first 6 characters of BNFCode
forvalues i= 6/8 {
	gen BNFCode`i'= substr(BNFCode, 1,`i')
}

*Make code in the same form as on SAIL e.g. 04.07.02.00
*replace digits 7 & 8 with 00 only
gen part1 =substr(BNFCode,1,2)
gen part2 =substr(BNFCode,3 ,2)
gen part3 =substr(BNFCode,5 ,2)
gen part4 =substr(BNFCode,7 ,2)
gen part400= ".00"
gen part5="."

gen BNFCODE= part1 + part5 + part2 + part5 + part3 + part400
drop part*

*Make variable of string length for SNOMED codes
gen BNFCodelength= strlen(SNOMEDCode)

*Use string length to work out which are Dmd and which are SNOMED
gen CodeType=""
replace CodeType="SNOMED" if BNFCodelength==9
replace CodeType="dmd+d" if BNFCodelength>9


*make binary variable for each drug
*NOTES all of the hydrochlorothiazides coded under one variable currently
gen	Alprazolam=1	if	 strpos(BNFName,"alprazo") | strpos(BNFName,"alprazolam") | strpos(BNFName,"xanax")
gen	Alverine=1	if strpos(BNFName,"alverin") | strpos(BNFName,"spasmonal") | strpos(BNFName,"audmonal")
gen	Amantadine=1	if  strpos(BNFName,"amantad") | strpos(BNFName,"symmetrel")
gen	Amisulpride=1	if strpos(BNFName,"amisulp") | strpos(BNFName,"solian")
gen	Amitriptyline=1	if strpos(BNFName,"amitrip")
gen	Aripiprazole=1	if strpos(BNFName,"aripipr")
gen	Atenolol=1	if strpos(BNFName,"atenolo") | strpos(BNFName,"tenif") ///
	| strpos(BNFName,"tenoretic")	| strpos(BNFName,"tenoret")	| strpos(BNFName,"tenormin")
gen	Atropine=1	 if strpos(BNFName,"atropin") | strpos(BNFName,"minims atropine sulfate")
gen	Baclofen=1	 if strpos(BNFName,"baclofe") | strpos(BNFName,"lioresal")
gen	Bendroflumethiazide	=1	 if  strpos(BNFName,"bendrof")
gen	Betamethasone=1	 if strpos(BNFName,"betamet") | strpos(BNFName,"enstiler") ///
	| strpos(BNFName,"vistamethasone")	| strpos(BNFName,"betesil")	| strpos(BNFName,"xemacort") ///
	| strpos(BNFName,"betnesol")	| strpos(BNFName,"betnovate")	| strpos(BNFName,"bettamousse")	| strpos(BNFName,"dovobet")	| strpos(BNFName,"lotriderm")
gen	Bumetanide=1 if  strpos(BNFName,"bumetan")
gen	Buprenorphine=1	 if 	 strpos(BNFName,"bupreno") | strpos(BNFName,"butec")	///
	| strpos(BNFName,"sevodyne")| strpos(BNFName,"butrans")	| strpos(BNFName,"transtec")	///
	| strpos(BNFName,"bupeaze")	| strpos(BNFName,"hapoctasin")	| strpos(BNFName,"teletrans")	///
	| strpos(BNFName,"turgeon")	| strpos(BNFName,"temgesic")	| strpos(BNFName,"buplast")	 ///
	| strpos(BNFName,"busiete")	| strpos(BNFName,"bunov")
gen	Captopril=1	if strpos(BNFName,"captopr") | strpos(BNFName,"capozide")
gen	Carbamazepine=1	if strpos(BNFName,"carbama") | strpos(BNFName,"tegretol") | strpos(BNFName,"carbagen")
gen	Cetirizine=1 if strpos(BNFName,"cetiriz") | strpos(BNFName,"zirtek")
gen Chlorphenamine=1 if strpos(BNFName, "chlorph") | strpos(BNFName,"piriton") | strpos(BNFName,"Haymine") ///
 | strpos(BNFName,"Hayleve")
gen	Chlorpromazine=1 if strpos(BNFName,"chlorpr") 
gen	Cimetidine=1 if strpos(BNFName,"cimetid")
gen	Cinnarizine=1 if strpos(BNFName,"cinnari")| strpos(BNFName,"sturgeon")	| strpos(BNFName,"arlevert") 
gen	Citalopram=1 if  strpos(BNFName,"citalop") | strpos(BNFName,"cipralex")	| strpos(BNFName,"cipramil")
gen	Clomipramine=1 if strpos(BNFName,"clomipr")
gen	Clonazepam=1 if strpos(BNFName,"clonaze")
gen	Clonidine=1	if strpos(BNFName,"clonidi") | strpos(BNFName,"dixarit")	| strpos(BNFName,"iopidine")
gen	Clozapine=1	if strpos(BNFName,"clozapi") | strpos(BNFName,"clozaril")
gen	Clobazam=1 if strpos(BNFName,"clobazam")
gen	Codeine=1 if strpos(BNFName,"codeine") | strpos(BNFName,"solpadol")	///
	| strpos(BNFName,"zapain")	| strpos(BNFName,"tylex")	| strpos(BNFName,"co-codamol")	///
	| strpos(BNFName,"migraleave")	| strpos(BNFName,"kapake")	| strpos(BNFName,"codipar") | strpos(BNFName,"solpadeine")
gen	Colchicine=1 if strpos(BNFName,"colchic")
gen	Cyclizine=1	if strpos(BNFName,"cyclizi")
gen	Darifenacin=1 if 	 strpos(BNFName,"darifen")
gen	Desloratadine=1	 if strpos(BNFName,"deslora") | strpos(BNFName,"neoclarityn")
gen	Dexamethasone=1	 if 	 strpos(BNFName,"dexamet") | strpos(BNFName,"otomize")	///
	| strpos(BNFName,"maxidex")	| strpos(BNFName,"adex")	| strpos(BNFName,"maxitrol")	///
	| strpos(BNFName,"dropodex")	| strpos(BNFName,"obradex")	| strpos(BNFName,"dexafree") ///
	| strpos(BNFName,"cilodex")
gen	Diamorphine=1 if strpos(BNFName,"diamorp") | strpos(BNFName,"ayendi")
gen	Diazepam=1 if strpos(BNFName,"diazepa")
gen	Dicycloverine=1	 if strpos(BNFName,"dicyclo")
gen	Digoxin	=1	if strpos(BNFName,"digoxin")
gen	Dihydrocodeine=1 if  strpos(BNFName,"dihydro") | strpos(BNFName,"remedeine") 	| strpos(BNFName,"dhc")	///
	| strpos(BNFName,"co-dydramol")
gen	Diphenhydramine=1 if strpos(BNFName,"diphenh") | strpos(BNFName,"nytol")
gen	Dipyridamole=1 if strpos(BNFName,"dipyrid") | strpos(BNFName,"persantin")
gen	Dosulepin=1	 if strpos(BNFName,"dosulep") 
gen	Doxazosin=1	 if strpos(BNFName,"doxazos") | strpos(BNFName,"cardura")	| strpos(BNFName,"doxadural")	///
	| strpos(BNFName,"larbex")
gen	Doxepin=1 if  strpos(BNFName,"doxepin") | strpos(BNFName,"xepin")
gen	Escitalopram=1 if  strpos(BNFName,"escital") | strpos(BNFName,"cipralex")
gen	Eslicarbazepine=1	if  strpos(BNFName,"eslicar") 
gen	Fentanyl=1	if 	 strpos(BNFName,"fentany") | strpos(BNFName,"mezolar") ///
	| strpos(BNFName,"matrifen") | strpos(BNFName,"durogesic")	| strpos(BNFName,"victanyl")	///
	| strpos(BNFName,"fencino")	| strpos(BNFName,"abstral")
gen	Fesoterodine=1 if strpos(BNFName,"fesoter") | strpos(BNFName,"toviaz")
gen	Fexofenadine=1	if strpos(BNFName,"fexofen") | strpos(BNFName,"telfast")
gen	Fluoxetine=1 if strpos(BNFName,"fluoxet") | strpos(BNFName,"prozac")
gen	Flupenthixol=1 if strpos(BNFName,"flupent") | strpos(BNFName,"Fluanxol") | strpos(BNFName,"depixol")
gen	Fluvoxamine=1 if strpos(BNFName,"fluvoxa")
gen	Furosemide=1 if strpos(BNFName,"furosem") | strpos(BNFName,"frumil")	///
	| strpos(BNFName,"frusene")	| strpos(BNFName,"frusol")	| strpos(BNFName,"co-amilofruse")
gen	Glycopyrronium_bromide=1 if strpos(BNFName,"glycopy") | strpos(BNFName,"seebri") | strpos(BNFName,"ultibro")
gen	Haloperidol	=1	if strpos(BNFName,"haloper") | strpos(BNFName,"haldol")
gen	Hydralazine	=1 if strpos(BNFName,"hydrala") | strpos(BNFName,"apresoline")
gen	Hydrochlorothiazide=1 if  strpos(BNFName,"hydrochlorothia") | strpos(BNFName,"innozide")	///
	| strpos(BNFName,"zestoretic")	| strpos(BNFName,"moduretic")	| strpos(BNFName,"coaprovel")	///
	| strpos(BNFName,"cozaar-comp")	| strpos(BNFName,"co-diovan")	| strpos(BNFName,"capozide")
gen	Hydrocortisone=1 if strpos(BNFName,"hydrocort") | strpos(BNFName,"germoloids hc")	///
	| strpos(BNFName,"perinal")	| strpos(BNFName,"plenadren")	| strpos(BNFName,"efcortesol")
gen	Hydromorphone=1	 if strpos(BNFName,"hydromo")
gen	Hydroxyzine=1 if strpos(BNFName,"hydroxyzine") | strpos(BNFName,"atarax")
gen	Hyoscine=1 if  strpos(BNFName,"hyoscin")
gen	Imipramine=1 if strpos(BNFName,"imipram")
gen	Indacaterol=1 if strpos(BNFName,"indacat") | strpos(BNFName,"ultibro")	| strpos(BNFName,"onbrez")
gen	Indapamide=1 if strpos(BNFName,"indapam") | strpos(BNFName,"natrilix")	| strpos(BNFName,"coversyl")	///
	| strpos(BNFName,"cardide")
gen	Indoramin=1	 if strpos(BNFName,"indoram") | strpos(BNFName,"doralese")
gen	Ipratropium=1 if strpos(BNFName,"ipratro") | strpos(BNFName,"atrovent")	| strpos(BNFName,"rinatex")	///
	| strpos(BNFName,"combivent")
gen	Isosorbide=1 if strpos(BNFName,"isosorb") | strpos(BNFName,"isotard")	| strpos(BNFName,"imdur")	///
	| strpos(BNFName,"elantan")	| strpos(BNFName,"monomax")	| strpos(BNFName,"nyzanac")	| strpos(BNFName,"ismo")	///
	| strpos(BNFName,"chemydur")	| strpos(BNFName,"tardisc")	| strpos(BNFName,"isoket")	| strpos(BNFName,"carmil")	| strpos(BNFName,"eumon")
gen	Levocetirizine=1 if strpos(BNFName,"levocet")
gen	Levomepromazine=1 if  strpos(BNFName,"levomep")
gen	Lofepramine=1 if strpos(BNFName,"lofepra")
gen	Loperamide=1 if strpos(BNFName,"loperam") | strpos(BNFName,"imodium")
gen	Loratadine=1 if strpos(BNFName,"loratad") | strpos(BNFName,"neoclrityn") | strpos(BNFName,"clarityn")
gen	Lorazepam=1	 if strpos(BNFName,"lorazep")
gen	Mebeverine=1 if strpos(BNFName,"mebever") | strpos(BNFName,"fybogel")	| strpos(BNFName,"colofac")
gen	Methocarbamol=1	if strpos(BNFName,"methoca") | strpos(BNFName,"robaxin")
gen	Metoprolol=1 if strpos(BNFName,"metopro")
gen	Midazolam=1	if strpos(BNFName,"midazol") | strpos(BNFName,"buccolam")	| strpos(BNFName,"epistatus")
gen	Mirtazapine=1 if strpos(BNFName,"mirtaza")
gen	Morphine=1 if strpos(BNFName,"morphin") | strpos(BNFName,"mst")	| strpos(BNFName,"oramorph") ///
	| strpos(BNFName,"zomorph")	| strpos(BNFName,"morphgesic")	| strpos(BNFName,"sevredol") ///
	| strpos(BNFName,"mxl")
gen	Naloxone=1	 if  strpos(BNFName,"naloxone")| strpos(BNFName,"targinact")
gen	Nefopam	=1	if  strpos(BNFName,"nefopam")
gen	Nifedipine=1 if strpos(BNFName,"nifedip") | strpos(BNFName,"nifedipress")	| strpos(BNFName,"adalat")	///
	| strpos(BNFName,"coracten")	| strpos(BNFName,"adipin")	| strpos(BNFName,"valni")	///
	| strpos(BNFName,"fortipine")	| strpos(BNFName,"nidef")	| strpos(BNFName,"tenif")	| strpos(BNFName,"tensipine")
gen	Nortriptyline=1	if strpos(BNFName,"nortrip")
gen	Olanzapine=1 if strpos(BNFName,"olanzap")
gen	Oxcarbazepine=1	if strpos(BNFName,"oxcarba")
gen	Oxybutynin=1 if strpos(BNFName,"oxybuty") | strpos(BNFName,"kentera")	///
	| strpos(BNFName,"ditropan")	| strpos(BNFName,"lyrinel")
gen	Oxycodone=1	if strpos(BNFName,"oxycodo") | strpos(BNFName,"oxycontin")	| strpos(BNFName,"reltebon")	///
	| strpos(BNFName,"oxynorm")	| strpos(BNFName,"longtec")	| strpos(BNFName,"shortec")	| strpos(BNFName,"lynlor")	///
	| strpos(BNFName,"targinact")
gen	Paroxetine=1 if strpos(BNFName,"paroxet") | strpos(BNFName,"seroxat")
gen	Prazosin=1	if strpos(BNFName,"prazosi")
gen	Prednisolone=1	 if strpos(BNFName,"prednis") | strpos(BNFName,"depo-medrone")	///
	| strpos(BNFName,"pred forte")	| strpos(BNFName,"scheriproct")	| strpos(BNFName,"predsol") 
gen	Prochlorperazine=1	 if strpos(BNFName,"prochlo") | strpos(BNFName,"buccastem")	| strpos(BNFName,"stemetil")
gen	Procyclidine=1	 if strpos(BNFName,"procycl") | strpos(BNFName,"kemadrin")
gen	Promazine=1	 if strpos(BNFName,"promazi") 
gen	Promethazine=1	if strpos(BNFName,"prometh") | strpos(BNFName,"phenergan")
gen	Propantheline_bromide=1	if strpos(BNFName,"propant") | strpos(BNFName,"pro-banthine")
gen	Quetiapine=1 if strpos(BNFName,"quetiap") | strpos(BNFName,"biquelle")	///
 | strpos(BNFName,"sondate")	| strpos(BNFName,"zaluron")	| strpos(BNFName,"seroquel")
gen	Ranitidine=1 if 	 strpos(BNFName,"ranitid")| strpos(BNFName,"zantac")
gen	Risperidone=1 if strpos(BNFName,"risperi")| strpos(BNFName,"risperdal")
gen	Solifenacin=1 if strpos(BNFName,"solifen")| strpos(BNFName,"vesicare")
gen	Sulpiride=1	 if strpos(BNFName,"sulpiri")| strpos(BNFName,"dolmatil")
gen	Tapentadol=1 if strpos(BNFName,"tapenta")
gen	Temazepam=1	 if strpos(BNFName,"temazep")
gen	Theophylline=1 if strpos(BNFName,"theophy") | strpos(BNFName,"uniphyllin")	| strpos(BNFName,"slo-phyllin")	///
	| strpos(BNFName,"nuelin")
gen	Tiotropium_bromide=1 if  strpos(BNFName,"tiotrop") | strpos(BNFName,"spiriva") 	| strpos(BNFName,"braltus")	///
	| strpos(BNFName,"spiolto")
gen	Tolterodine	=1 if strpos(BNFName,"toltero") | strpos(BNFName,"neditol")	| strpos(BNFName,"mariosea") ///
	| strpos(BNFName,"detrusitol")
gen	Tramadol=1	 if strpos(BNFName,"tramado") | strpos(BNFName,"mabron")	| strpos(BNFName,"marol") ///
	| strpos(BNFName,"maxitram")	| strpos(BNFName,"zydol")	| strpos(BNFName,"tramcet")	///
	| strpos(BNFName,"tramulief")	| strpos(BNFName,"tradorec")	| strpos(BNFName,"zamadol")	| strpos(BNFName,"tramquel")
gen	Trazodone=1	 if strpos(BNFName,"trazodo") | strpos(BNFName,"molipaxin")
gen	Trihexyphenidyl=1 if strpos(BNFName,"trihexy")
gen	Trimipramine=1	 if strpos(BNFName,"trimipram")
gen	Trospium_chloride=1	 if  strpos(BNFName,"trospiu") | strpos(BNFName,"flotros")	| strpos(BNFName,"regurin")
gen	Umeclidinium=1	if strpos(BNFName,"umeclid") | strpos(BNFName,"incruse ellipta")	///
	| strpos(BNFName,"anoro ellipta")	| strpos(BNFName,"trelegy")
gen	Venlafaxine=1 if  strpos(BNFName,"venlafa") | strpos(BNFName,"vensir")	| strpos(BNFName,"rodomel")	| strpos(BNFName,"viepax")	| strpos(BNFName,"venlablue") | strpos(BNFName,"venlalic")	| strpos(BNFName,"efexor")
gen	Warfarin=1 if strpos(BNFName,"warfari")

foreach x of varlist Alprazolam-Warfarin {
	replace `x' = 0 if `x'==.
}

gen Drug=""
replace Drug="Alprazolam"	if	 strpos(BNFName,"alprazo") | strpos(BNFName,"alprazolam") | strpos(BNFName,"xanax")
replace Drug="Alverine"	if strpos(BNFName,"alverin") | strpos(BNFName,"spasmonal") | strpos(BNFName,"audmonal")
replace Drug="Amantadine"	if  strpos(BNFName,"amantad") | strpos(BNFName,"symmetrel")
replace Drug="Amisulpride"	if strpos(BNFName,"amisulp") | strpos(BNFName,"solian")
replace Drug="Amitriptyline"	if strpos(BNFName,"amitrip")
replace Drug="Aripiprazole"	if strpos(BNFName,"aripipr")
replace Drug="Atenolol"	if strpos(BNFName,"atenolo") | strpos(BNFName,"tenif") ///
	| strpos(BNFName,"tenoretic")	| strpos(BNFName,"tenoret")	| strpos(BNFName,"tenormin")
replace Drug="Atropine"	 if strpos(BNFName,"atropin") | strpos(BNFName,"minims atropine sulfate")
replace Drug="Baclofen"	 if strpos(BNFName,"baclofe") | strpos(BNFName,"lioresal")
replace Drug="Bendroflumethiazide	"	 if  strpos(BNFName,"bendrof")
replace Drug="Betamethasone"	 if strpos(BNFName,"betamet") | strpos(BNFName,"enstiler") ///
	| strpos(BNFName,"vistamethasone")	| strpos(BNFName,"betesil")	| strpos(BNFName,"xemacort") ///
	| strpos(BNFName,"betnesol")	| strpos(BNFName,"betnovate")	| strpos(BNFName,"bettamousse")	| strpos(BNFName,"dovobet")	| strpos(BNFName,"lotriderm")
replace Drug="Bumetanide" if  strpos(BNFName,"bumetan")
replace Drug="Buprenorphine"	 if 	 strpos(BNFName,"bupreno") | strpos(BNFName,"butec")	///
	| strpos(BNFName,"sevodyne")| strpos(BNFName,"butrans")	| strpos(BNFName,"transtec")	///
	| strpos(BNFName,"bupeaze")	| strpos(BNFName,"hapoctasin")	| strpos(BNFName,"teletrans")	///
	| strpos(BNFName,"turgeon")	| strpos(BNFName,"temgesic")	| strpos(BNFName,"buplast")	 ///
	| strpos(BNFName,"busiete")	| strpos(BNFName,"bunov")
replace Drug="Captopril"	if strpos(BNFName,"captopr") | strpos(BNFName,"capozide")
replace Drug="Carbamazepine"	if strpos(BNFName,"carbama") | strpos(BNFName,"tegretol") | strpos(BNFName,"carbareplace")
replace Drug="Cetirizine" if strpos(BNFName,"cetiriz") | strpos(BNFName,"zirtek")
replace Drug="Chlorphenamine" if strpos(BNFName, "chlorph") | strpos(BNFName,"piriton") | strpos(BNFName,"Haymine") ///
 | strpos(BNFName,"Hayleve")
replace Drug="Chlorpromazine" if strpos(BNFName,"chlorpr") 
replace Drug="Cimetidine" if strpos(BNFName,"cimetid")
replace Drug="Cinnarizine" if strpos(BNFName,"cinnari")| strpos(BNFName,"sturgeon")	| strpos(BNFName,"arlevert") 
replace Drug="Citalopram" if  strpos(BNFName,"citalop") | strpos(BNFName,"cipralex")	| strpos(BNFName,"cipramil")
replace Drug="Clomipramine" if strpos(BNFName,"clomipr")
replace Drug="Clonazepam" if strpos(BNFName,"clonaze")
replace Drug="Clonidine"	if strpos(BNFName,"clonidi") | strpos(BNFName,"dixarit")	| strpos(BNFName,"iopidine")
replace Drug="Clozapine"	if strpos(BNFName,"clozapi") | strpos(BNFName,"clozaril")
replace Drug="Clobazam" if strpos(BNFName,"clobazam")
replace Drug="Codeine" if strpos(BNFName,"codeine") | strpos(BNFName,"solpadol")	///
	| strpos(BNFName,"zapain")	| strpos(BNFName,"tylex")	| strpos(BNFName,"co-codamol")	///
	| strpos(BNFName,"migraleave")	| strpos(BNFName,"kapake")	| strpos(BNFName,"codipar") | strpos(BNFName,"solpadeine")
replace Drug="Colchicine" if strpos(BNFName,"colchic")
replace Drug="Cyclizine"	if strpos(BNFName,"cyclizi")
replace Drug="Darifenacin" if 	 strpos(BNFName,"darifen")
replace Drug="Desloratadine"	 if strpos(BNFName,"deslora") | strpos(BNFName,"neoclarityn")
replace Drug="Dexamethasone"	 if 	 strpos(BNFName,"dexamet") | strpos(BNFName,"otomize")	///
	| strpos(BNFName,"maxidex")	| strpos(BNFName,"adex")	| strpos(BNFName,"maxitrol")	///
	| strpos(BNFName,"dropodex")	| strpos(BNFName,"obradex")	| strpos(BNFName,"dexafree") ///
	| strpos(BNFName,"cilodex")
replace Drug="Diamorphine" if strpos(BNFName,"diamorp") | strpos(BNFName,"ayendi")
replace Drug="Diazepam" if strpos(BNFName,"diazepa")
replace Drug="Dicycloverine"	 if strpos(BNFName,"dicyclo")
replace Drug="Digoxin	"	if strpos(BNFName,"digoxin")
replace Drug="Dihydrocodeine" if  strpos(BNFName,"dihydro") | strpos(BNFName,"remedeine") 	| strpos(BNFName,"dhc")	///
	| strpos(BNFName,"co-dydramol")
replace Drug="Diphenhydramine" if strpos(BNFName,"diphenh") | strpos(BNFName,"nytol")
replace Drug="Dipyridamole" if strpos(BNFName,"dipyrid") | strpos(BNFName,"persantin")
replace Drug="Dosulepin"	 if strpos(BNFName,"dosulep") 
replace Drug="Doxazosin"	 if strpos(BNFName,"doxazos") | strpos(BNFName,"cardura")	| strpos(BNFName,"doxadural")	///
	| strpos(BNFName,"larbex")
replace Drug="Doxepin" if  strpos(BNFName,"doxepin") | strpos(BNFName,"xepin")
replace Drug="Escitalopram" if  strpos(BNFName,"escital") | strpos(BNFName,"cipralex")
replace Drug="Eslicarbazepine"	if  strpos(BNFName,"eslicar") 
replace Drug="Fentanyl"	if 	 strpos(BNFName,"fentany") | strpos(BNFName,"mezolar") ///
	| strpos(BNFName,"matrifen") | strpos(BNFName,"durogesic")	| strpos(BNFName,"victanyl")	///
	| strpos(BNFName,"fencino")	| strpos(BNFName,"abstral")
replace Drug="Fesoterodine" if strpos(BNFName,"fesoter") | strpos(BNFName,"toviaz")
replace Drug="Fexofenadine"	if strpos(BNFName,"fexofen") | strpos(BNFName,"telfast")
replace Drug="Fluoxetine" if strpos(BNFName,"fluoxet") | strpos(BNFName,"prozac")
replace Drug="Flupenthixol" if strpos(BNFName,"flupent") | strpos(BNFName,"Fluanxol") | strpos(BNFName,"depixol")
replace Drug="Fluvoxamine" if strpos(BNFName,"fluvoxa")
replace Drug="Furosemide" if strpos(BNFName,"furosem") | strpos(BNFName,"frumil")	///
	| strpos(BNFName,"frusene")	| strpos(BNFName,"frusol")	| strpos(BNFName,"co-amilofruse")
replace Drug="Glycopyrronium_bromide" if strpos(BNFName,"glycopy") | strpos(BNFName,"seebri") | strpos(BNFName,"ultibro")
replace Drug="Haloperidol	"	if strpos(BNFName,"haloper") | strpos(BNFName,"haldol")
replace Drug="Hydralazine	" if strpos(BNFName,"hydrala") | strpos(BNFName,"apresoline")
replace Drug="Hydrochlorothiazide" if  strpos(BNFName,"hydrochlorothia") | strpos(BNFName,"innozide")	///
	| strpos(BNFName,"zestoretic")	| strpos(BNFName,"moduretic")	| strpos(BNFName,"coaprovel")	///
	| strpos(BNFName,"cozaar-comp")	| strpos(BNFName,"co-diovan")	| strpos(BNFName,"capozide")
replace Drug="Hydrocortisone" if strpos(BNFName,"hydrocort") | strpos(BNFName,"germoloids hc")	///
	| strpos(BNFName,"perinal")	| strpos(BNFName,"plenadren")	| strpos(BNFName,"efcortesol")
replace Drug="Hydromorphone"	 if strpos(BNFName,"hydromo")
replace Drug="Hydroxyzine" if strpos(BNFName,"hydroxyzine") | strpos(BNFName,"atarax")
replace Drug="Hyoscine" if  strpos(BNFName,"hyoscin")
replace Drug="Imipramine" if strpos(BNFName,"imipram")
replace Drug="Indacaterol" if strpos(BNFName,"indacat") | strpos(BNFName,"ultibro")	| strpos(BNFName,"onbrez")
replace Drug="Indapamide" if strpos(BNFName,"indapam") | strpos(BNFName,"natrilix")	| strpos(BNFName,"coversyl")	///
	| strpos(BNFName,"cardide")
replace Drug="Indoramin"	 if strpos(BNFName,"indoram") | strpos(BNFName,"doralese")
replace Drug="Ipratropium" if strpos(BNFName,"ipratro") | strpos(BNFName,"atrovent")	| strpos(BNFName,"rinatex")	///
	| strpos(BNFName,"combivent")
replace Drug="Isosorbide" if strpos(BNFName,"isosorb") | strpos(BNFName,"isotard")	| strpos(BNFName,"imdur")	///
	| strpos(BNFName,"elantan")	| strpos(BNFName,"monomax")	| strpos(BNFName,"nyzanac")	| strpos(BNFName,"ismo")	///
	| strpos(BNFName,"chemydur")	| strpos(BNFName,"tardisc")	| strpos(BNFName,"isoket")	| strpos(BNFName,"carmil")	| strpos(BNFName,"eumon")
replace Drug="Levocetirizine" if strpos(BNFName,"levocet")
replace Drug="Levomepromazine" if  strpos(BNFName,"levomep")
replace Drug="Lofepramine" if strpos(BNFName,"lofepra")
replace Drug="Loperamide" if strpos(BNFName,"loperam") | strpos(BNFName,"imodium")
replace Drug="Loratadine" if strpos(BNFName,"loratad") | strpos(BNFName,"neoclrityn") | strpos(BNFName,"clarityn")
replace Drug="Lorazepam"	 if strpos(BNFName,"lorazep")
replace Drug="Mebeverine" if strpos(BNFName,"mebever") | strpos(BNFName,"fybogel")	| strpos(BNFName,"colofac")
replace Drug="Methocarbamol"	if strpos(BNFName,"methoca") | strpos(BNFName,"robaxin")
replace Drug="Metoprolol" if strpos(BNFName,"metopro")
replace Drug="Midazolam"	if strpos(BNFName,"midazol") | strpos(BNFName,"buccolam")	| strpos(BNFName,"epistatus")
replace Drug="Mirtazapine" if strpos(BNFName,"mirtaza")
replace Drug="Morphine" if strpos(BNFName,"morphin") | strpos(BNFName,"mst")	| strpos(BNFName,"oramorph") ///
	| strpos(BNFName,"zomorph")	| strpos(BNFName,"morphgesic")	| strpos(BNFName,"sevredol") ///
	| strpos(BNFName,"mxl")
replace Drug="Naloxone"	 if  strpos(BNFName,"naloxone")| strpos(BNFName,"targinact")
replace Drug="Nefopam	"	if  strpos(BNFName,"nefopam")
replace Drug="Nifedipine" if strpos(BNFName,"nifedip") | strpos(BNFName,"nifedipress")	| strpos(BNFName,"adalat")	///
	| strpos(BNFName,"coracten")	| strpos(BNFName,"adipin")	| strpos(BNFName,"valni")	///
	| strpos(BNFName,"fortipine")	| strpos(BNFName,"nidef")	| strpos(BNFName,"tenif")	| strpos(BNFName,"tensipine")
replace Drug="Nortriptyline"	if strpos(BNFName,"nortrip")
replace Drug="Olanzapine" if strpos(BNFName,"olanzap")
replace Drug="Oxcarbazepine"	if strpos(BNFName,"oxcarba")
replace Drug="Oxybutynin" if strpos(BNFName,"oxybuty") | strpos(BNFName,"kentera")	///
	| strpos(BNFName,"ditropan")	| strpos(BNFName,"lyrinel")
replace Drug="Oxycodone"	if strpos(BNFName,"oxycodo") | strpos(BNFName,"oxycontin")	| strpos(BNFName,"reltebon")	///
	| strpos(BNFName,"oxynorm")	| strpos(BNFName,"longtec")	| strpos(BNFName,"shortec")	| strpos(BNFName,"lynlor")	///
	| strpos(BNFName,"targinact")
replace Drug="Paroxetine" if strpos(BNFName,"paroxet") | strpos(BNFName,"seroxat")
replace Drug="Prazosin"	if strpos(BNFName,"prazosi")
replace Drug="Prednisolone"	 if strpos(BNFName,"prednis") | strpos(BNFName,"depo-medrone")	///
	| strpos(BNFName,"pred forte")	| strpos(BNFName,"scheriproct")	| strpos(BNFName,"predsol") 
replace Drug="Prochlorperazine"	 if strpos(BNFName,"prochlo") | strpos(BNFName,"buccastem")	| strpos(BNFName,"stemetil")
replace Drug="Procyclidine"	 if strpos(BNFName,"procycl") | strpos(BNFName,"kemadrin")
replace Drug="Promazine"	 if strpos(BNFName,"promazi") & Drug==""
replace Drug="Promethazine"	if strpos(BNFName,"prometh") | strpos(BNFName,"phenergan")
replace Drug="Propantheline_bromide"	if strpos(BNFName,"propant") | strpos(BNFName,"pro-banthine")
replace Drug="Quetiapine" if strpos(BNFName,"quetiap") | strpos(BNFName,"biquelle")	///
 | strpos(BNFName,"sondate")	| strpos(BNFName,"zaluron")	| strpos(BNFName,"seroquel")
replace Drug="Ranitidine" if 	 strpos(BNFName,"ranitid")| strpos(BNFName,"zantac")
replace Drug="Risperidone" if strpos(BNFName,"risperi")| strpos(BNFName,"risperdal")
replace Drug="Solifenacin" if strpos(BNFName,"solifen")| strpos(BNFName,"vesicare")
replace Drug="Sulpiride"	 if strpos(BNFName,"sulpiri")| strpos(BNFName,"dolmatil")
replace Drug="Tapentadol" if strpos(BNFName,"tapenta")
replace Drug="Temazepam"	 if strpos(BNFName,"temazep")
replace Drug="Theophylline" if strpos(BNFName,"theophy") | strpos(BNFName,"uniphyllin")	| strpos(BNFName,"slo-phyllin")	///
	| strpos(BNFName,"nuelin")
replace Drug="Tiotropium_bromide" if  strpos(BNFName,"tiotrop") | strpos(BNFName,"spiriva") 	| strpos(BNFName,"braltus")	///
	| strpos(BNFName,"spiolto")
replace Drug="Tolterodine	" if strpos(BNFName,"toltero") | strpos(BNFName,"neditol")	| strpos(BNFName,"mariosea") ///
	| strpos(BNFName,"detrusitol")
replace Drug="Tramadol"	 if strpos(BNFName,"tramado") | strpos(BNFName,"mabron")	| strpos(BNFName,"marol") ///
	| strpos(BNFName,"maxitram")	| strpos(BNFName,"zydol")	| strpos(BNFName,"tramcet")	///
	| strpos(BNFName,"tramulief")	| strpos(BNFName,"tradorec")	| strpos(BNFName,"zamadol")	| strpos(BNFName,"tramquel")
replace Drug="Trazodone"	 if strpos(BNFName,"trazodo") | strpos(BNFName,"molipaxin")
replace Drug="Trihexyphenidyl" if strpos(BNFName,"trihexy")
replace Drug="Trimipramine"	 if strpos(BNFName,"trimipram")
replace Drug="Trospium_chloride"	 if  strpos(BNFName,"trospiu") | strpos(BNFName,"flotros")	| strpos(BNFName,"regurin")
replace Drug="Umeclidinium"	if strpos(BNFName,"umeclid") | strpos(BNFName,"incruse ellipta")	///
	| strpos(BNFName,"anoro ellipta")	| strpos(BNFName,"trelegy")
replace Drug="Venlafaxine" if  strpos(BNFName,"venlafa") | strpos(BNFName,"vensir")	| strpos(BNFName,"rodomel")	| strpos(BNFName,"viepax")	| strpos(BNFName,"venlablue") | strpos(BNFName,"venlalic")	| strpos(BNFName,"efexor")
replace Drug="Warfarin" if strpos(BNFName,"warfari")


*keep only those with Drug listed
gen keep=0
foreach x of varlist Alprazolam- Warfarin {
replace keep =1 if `x'==1	
}
tab  Drug keep, m 
keep if Drug!=""

*Check those with diamorphine haven't been replaced by morphine
replace Drug="Diamorphine" if Diamorphine==1
replace Morphine=0 if Drug=="Diamorphine"

*Check those with chloropromazine and levopromazine aren't replaced by Promazine
replace Drug="Chlorpromazine" if Chlorpromazine==1
replace Promazine=0 if Chlorpromazine==1

replace Drug="Levomepromazine" if Levomepromazine==1
replace Promazine=0 if Levomepromazine==1


*marker those with cream/ gel/ foam/ ointment/ eye drops in title?
* go through once in excel to double check
gen exclude=0
replace exclude=1 if  strpos(BNFName,"cream") 
replace exclude=1 if  strpos(BNFName,"crm") 	
replace exclude=1 if  strpos(BNFName,"lotion") 	
replace exclude=1 if  strpos(BNFName,"gel") 	
replace exclude=1 if  strpos(BNFName,"ointment") 	
 	
*Get rid of apomorphine as this isn't included but contains word morphine
replace exclude=1 if strpos(BNFName,"apomorphine")
keep if exclude==0


order Drug, before(Alprazolam)
sort Drug BNFName


*Identify those with a \ or / as need to split into 2 bits of info
gen BNFName2=BNFName
split BNFName2 , p("\" "/")

*extract only numbers. 
destring BNFName21 , gen(Dose1) i(a b c d e f g h i j k l m n o p q r s t u v w x y z % ( ) - _ * ) force
destring BNFName22 , gen(Dose2) i(a b c d e f g h i j k l m n o p q r s t u v w x y z % ( ) - _ * ) force
destring BNFName23 , gen(Dose3) i(a b c d e f g h i j k l m n o p q r s t u v w x y z % ( ) - _ * ) force
destring BNFName24 , gen(Dose4) i(a b c d e f g h i j k l m n o p q r s t u v w x y z % ( ) - _ * ) force

keep VMPVMPPAMPAMPP  BNFName SNOMEDCode DMDProductDescription BNFCODE CodeType Drug Alprazolam Alverine-Warfarin Dose*

sort Drug SNOMEDCode
* drug codes
export excel using "AC medication codes 2.xlsx", sheet("All drugs") cell(A1) firstrow(variables) replace




