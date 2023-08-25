---
title: 'BioHackJP 2023 Report R3: Plant data integration for findability across multiple databases'
title_short: 'BioHackJP 2023 Plants'
tags:
  - Linked Data
  - Plant genome
  - Plant breeding
authors:
  - name: Cyril Pommier
    orcid: 0000-0002-9040-8733
    affiliation: 1, 2
  - name: Hiromi Kajiya-Kanegae
    orcid: 0000-0002-5719-7559
    affiliation: 3
  - name: Andrea Ghelfi
    orcid: 0000-0001-9617-3309
    affiliation: 4
affiliations:
  - name: Université Paris-Saclay, INRAE, URGI, 78026, Versailles, France.
    index: 1
  - name: Université Paris-Saclay, INRAE, BioinfOmics, Plant bioinformatics facility, 78026, Versailles, France.
    index: 2
  - name: Research Center for Agricultural Information Technology (NARO), Tokyo, 105-0003, Japan
    index: 3
  - name: National Institute of Genetics (NIG), Shizuoka, 411-8540, Japan.
    index: 4
date: 30 June 2023
cito-bibliography: paper.bib
event: BH23JP
biohackathon_name: "BioHackathon Japan 2023"
biohackathon_url:   "https://2023.biohackathon.org/"
biohackathon_location: "Kagawa, Japan, 2023"
group: R3
# URL to project git repo --- should contain the actual paper.md:
git_url: https://github.com/biohackathon-japan/bh23-plant
# This is the short authors description that is used at the
# bottom of the generated paper (typically the first two authors):
authors_short: Ghelfi A., Kajiya-Kanegae H, Pommier C.
---

# Background

The field of plant research is characterized by vast amounts of data generated from diverse sources and studies, resulting in significant data dispersion and heterogeneity. Researchers often face challenges in accessing, integrating, and analyzing these disparate datasets due to their low findability and to differences in formats, structures, and standards. There are several solutions to this problem that cope with different parts of the FAIR principles. The plant community has decided in particular to solve the interoperability and the findability problem by building common data standards such as the Minimum Information About Plant Phenotyping Experiments (MIAPPE, www.miappe.org) (Papoutsoglou et al., 2020) and the Breeding API (BrAPI, www.brapi.org) (Selby et al., 2019) as well as a data portal, FAIR Data-finder for Agronomic Research (FAIDARE). The building in those solutions has been initiated around 2015 with a global perspective in mind in the frame of european infrastructures (ELIXIR, EMPHASIS) (Harrow et al., 2021) and international networks(Bioversity of the CGIAR, NAPPN). The BioHackathon Japan 2023, was an ideal event to outreach those solutions toward the Japanese researchers and bioinformaticians in order to increase visibility of Japanese databases in the plant research data discovery portal FAIDARE and explore the use of the Breeding API for knowledge graph. 

The FAIDARE data portal has been initiated in 2015 in the frame of the ELIXIR European research infrastructure and the wheatIS of the wheat initiative (https://urgi.versailles.inrae.fr/faidare/ , http://www.wheatis.org/Search.php). It indexes 30 databases across the globe, including genomic, genetic and phenomic data. It provides search capabilities using full text approaches based on Elasticsearch, as well as ontology driven filters tailored for the need of plant researchers.  One of the keys to its success is its community management that eases the indexation of new databases (https://urgi.versailles.inrae.fr/faidare/join). Indeed, this indexation relies on (i) a minimal approach (Data discovery files) that allows to index any datatype or (ii) a more detailed one, based on the Breeding API specifications for genetic material (germplasm) and phenotyping or genotyping studies. 

The MIAPPE data standard is intialy designed to describe and formalize plant phenomics studies. But some of its elements are important shared key resources, or interoperability pivot. It is in particular the case of the plant genetic resources, or biological material. As a consequence, the Biological section of MIAPPE has been embeded in a dedicated [checklist](https://www.ebi.ac.uk/biosamples/schemas/certification/plant-miappe.json), used during EBI Biosample submission. Its usage has been documented in the Research Data Management Toolkit RDMKit and the FAIRCookBook. The [RDMKit](https://rdmkit.elixir-europe.org/) is a portal to best practices and guidelines that help researchers, data steward, producer and scientist to make their data FAIR (Findable, Accessible, Interoperable and Reusable). It contains a community maintained [plant page](https://rdmkit.elixir-europe.org/plant_sciences) that should be used as an entry point for plant data management. Furthermore, it links to a [Phenomic tools assembly](https://rdmkit.elixir-europe.org/plant_phenomics_assembly) and to a [Genomic tool assembly](https://rdmkit.elixir-europe.org/plant_genomics_assembly) which describe in details the submission workflow of genomic and variation data at EBI ENA and EVA.

We also identified an interesting opportunity around pedigrees and graph databases. Indeed several data resources around the globe propose pedigree informations such as Pedigree Finder (Kajiya-Kanegae et al., 2022), [UK wheat varieties pedigree](https://www.niab.com/uk-wheat-varieties-pedigree), FAIDARE, [CTPS](https://www.geves.fr/about-us/the-ctps/) or [Eurisco](https://eurisco.ipk-gatersleben.de/apex/eurisco_ws/r/eurisco/home) in format such as the [Helium data format](https://github.com/cardinalb/helium-docs/wiki/Download-Helium) or BrAPI. 



# Outcomes

The main goal is to integrate germoplasm and genomic data in Japan at FAIDARE. Therefore, a mapping has been performed between BioSample database at DDBJ with MIAPPE specifications based on RDMkit and FAIRCookBook. Mostly the attributes exist in both systems, with a different terms (Table) and we are proposing to create a consensus MixS Plant extended accordingly with the Genomic Standards Consortioum ([GSC](http://www.gensc.org/pages/standards/checklists.html)) [checklists](http://www.gensc.org/pages/standards/checklists.html). We are proposing to create new fields at the [DDBJ BioSample package](https://github.com/ddbj/pub/tree/master/docs/biosample) in order to follow MIAPPE such as, biological material (latitute, longitude, altitude, coordinates uncertainty and preprocessing) and material source (ID, DOI, description, latitute, longitude, altitude, coordinates uncertainty). Some changes at DDBJ BioSample package were suggested, the feature organism be split into genus and species, and unify strain, isolate, breed and cultivar as infraspecific name. Note that Biosample DDBJ packages are equivalent to EBI Checklists. 

We started the integration using a rice cultivar sample extracted from the Rice Core Collection of Japanese Landraces [NARO Genebank](https://www.gene.affrc.go.jp/databases-core_collections_jr_en.php), and BioSample ids from DDBJ using taxon id and cultivar name. From DDBJ information such as experiments, study name, cultivar name and DDBJ URI were integrated with germoplasm id and URI, from NARO. This NARO rice dataset was selected to test the indexation of DDBJ biosample and experiment in the FAIDARE data portal. Two files were generated in json format, germplasm and study. The former contains germplasm id, which were associate with BioSample id from DDBJ, experiment, germplasm name, taxon id from NCBI taxon, genus, species and variety names, germplasm URI (which links to the correspondent DDBJ BioSample), holding institute name responsible for submitting the genomic data to DDBJ, project id and external reference which links to the NARO Genbank URI. The later has experiment id from DDBJ with study name, study description, last update, submitter name and URI refering DDBJ experiment.

Those data were then tested in a local instance of FAIDARE, alongside INRAE data. This allowed to succesfully doing full text search in all the fields and to filter by species, database, datatype. Following this demonstration, the data will be loaded in the beta instance of FAIDARE. It will then be possible to extract the whole plant dataset from DDBJ and insert those metadata into the production instance of FAIDARE.



We also initated the work on knowledge graph building by reviewing some the potential datasets previously identified, including aditional datasources to provide linked IDs between DDBJ Biosample, NARO, [Plant GARDEN](https://plantgarden.jp/en/index) at Kazusa DNA Research Institute. Nonetheless, a more exhaustive review of the data and linked ID resources is necessary before carrying on that work. 





# Future work
We will finalize the DDBJ databases indexation in FAIDARE public instance after validation by DDBJ teams.
We plan to improve the Biosample Checklist for at least EBI and DDBJ, by elaborating on ENA MixS and MIAPPE, used at EBI Biosample/EVA, in this direction, a workgroup was already created between ELIXIR and DDBJ. We plan to improve the attributes (terms) to enrich the indexation in DDBJ@FAIDARE and make regular releases. 
We also plan to explore pedigree knowledge graph based on Japanese and European data source, as well as, identify and prepare datasets for an upcoming biohackathon.



## Acknowledgements

We would like to thank the fellow participants at BioHackathon 2023 for their collaboration and constructive advice, which greatly influenced our project. We are grateful to the organizers for providing this platform. Special thanks to our mentors, advisors, and colleagues for their guidance and support. Without their contributions, our project in plant data integration would not have been possible.

## References

Harrow J, Hancock J, ELIXIR-EXCELERATE Community, et al. (2021) ELIXIR-EXCELERATE: establishing Europe’s data infrastructure for the life science research of the future. The EMBO journal 40(6), e107409. [doi:10.15252/embj.2020107409](https://doi.org/10.15252/embj.2020107409)

Kajiya-Kanegae H, Matsushita K, Hayashi T, et al. (2022) Pedigree Finder: A web-based crop pedigree viewer for graph databases. Breeding Research 24(2), 115–123. [10.1270/jsbbr.22J02](https://doi.org/10.1270/jsbbr.22J02)

Papoutsoglou EA, Faria D, Arend D, et al. (2020) Enabling reusability of plant phenomic datasets with MIAPPE 1.1. The New phytologist 227(1), 260–273. [doi:10.1111/nph.16544]( https://doi.org/10.1111/nph.16544)

Selby P, Abbeloos R, Backlund JE, et al. (2019) BrAPI—an application programming interface for plant breeding applications. Bioinformatics 35(20), 4147–4155. [doi:10.1093/bioinformatics/btz190](https://doi.org/10.1093/bioinformatics/btz190)

## Annexes 



**Table:Mapping DDBJ Biosample plant to ENA Plant MIAPPE Checklist**
https://github.com/ddbj/pub/tree/master/docs/biosample
https://www.ebi.ac.uk/biosamples/schemas/certification/plant-miappe.json

|Field from DDBJ corresponding to the checklist|DDBJ Biosample Plants <br>DDBJ new package Field Name|ENA Plant MIAPPE Checklist <br>Field name|ENA Plant MIAPPE Checklist <br>comment|ENA Plant MIAPPE Checklist <br>Cardinality|ENA Plant MIAPPE Checklist <br>source|
|------|------|------|------|------|------|
||bioproject_id|project name|Introduce the notion of investigation ? A project can have several investigation|optional|Gsc MIxS|
||sample_name|(DM-41) Biological material ID||mandatory|MIAPPE|
||taxonomy_id|(DM-42) Organism||mandatory|MIAPPE|
|organism|genus|(DM-43) Genus||optional|MIAPPE|
||species|(DM-44) Species||optional|MIAPPE|
|strain, isolate, breed, cultivar|infraspecific_name|(DM-44') Infraspecific name||recommended|MIAPPE|
||ecotype|ecotype||optional|ENA Plant|
||biological_material_latitute|(DM-45) Biological material latitude||optional|MIAPPE|
||biological_material_longitude|(DM-46) Biological material longitude||optional|MIAPPE|
||biological_material_altitude|(DM-47) Biological material altitude||optional|MIAPPE|
||biological_material_coordinates_uncertainty|(DM-48) Biological material coordinates uncertainty||optional|MIAPPE|
||geo_loc_name|Biological material geographic location (country and/or sea)|To be discussed|optional|MIAPPE|
||biological_material_preprocessing|(DM-49) Biological material preprocessing||optional|MIAPPE|
||material_source_ID|(DM-50) Material source ID (Holding institute/stock centre, accession)||recommended|MIAPPE|
||material_source_DOI|(DM-51) Material source DOI||recommended|MIAPPE|
||material_source_latitute|(DM-52) Material source latitude||optional|MIAPPE|
||material_source_longitude|(DM-53) Material source longitude||optional|MIAPPE|
||material_source_altitude|(DM-54) Material source altitude||optional|MIAPPE|
||material_souce_coordinates_uncertainty|(DM-55) Material source coordinates uncertainty||optional|MIAPPE|
||material_source_description|(DM-56) Material source description||optional|MIAPPE|
||Material source geographic location|Material source geographic location (country and/or sea)|To be discussed|optional|MIAPPE|
||ploidy|Biological material ploidy|should us ethe existing "ploidy" field instead|recommended|ENA Plant|
||collected_by|collected_by||optional|ENA Plant|
||collection_date|collection date||optional|ENA Plant, MIAPPE DM-80|
||sex|sex||optional|ENA Crop Plant|
||env_broad_scale|environment (biome)||optional|ENA Plant|
||dev_stage|DM-77 Plant structure development
stage||optional|MIAPPE|
||host_anatomical_material|DM-78 Plant anatomical entity||optional|MIAPPE|
||description|DM-79 Sample description|Or replace with the many fields taken from ENA Plant Sample|recommended|MIAPPE|
||sample_id|DM-75 Sample ID|List of IDs, different from BiosampleID, used for interoperability and for linking sample within different datafiles|recommended|MIAPPE/ENA|
||env_parameter|Environment parameter|Environment parameter = Environment parameter value|optional|MIAPPE|
||Material Source Institute code|DM-xx95 Material Source Institute code|FAO WIEWS code of the institute where the accession is maintained. The codes consist of the 3-letter ISO 3166 country code of the country where the institute is located plus a number (e.g. COL001). The current set of institute codes is available from http://www.fao.org/wiews. For those institutes not yet having an FAO Code, or for those with ‘obsolete’ codes, see ‘Common formatting rules (v)’. If no Institue code, create your own (Laboratory accronyme, Research Institute Acronyme, ...).|||
||Material Source Accession number|DM-xx96 Material Source Accession number|This is the unique identifier for accessions within a genebank, and is assigned when a sample is entered into the genebank collection (e.g. ‘PI 113869’). If not a genebank, use a laboratory ID. In the case of a commercial Variety, use the variety anme or code.|||
