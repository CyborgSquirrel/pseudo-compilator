import pathlib

import polars as pl
from xlsx2csv import Xlsx2csv

DATA = pathlib.Path("data")

SUBJECT_MAP = {
    "Chimie organică TEO Nivel I/II"                           : "Chemistry"  ,
    "Anatomie și fiziologie umană, genetică și ecologie umană" : "Anatomy"    ,
    "Geografie"                                                : "Geography"  ,
    "Economie"                                                 : "Economy"    ,
    "Psihologie"                                               : "Psychology" ,
    "Informatică SN Pascal"                                    : "Informatics",
    "Informatică SN C/C++"                                     : "Informatics",
    "Biologie vegetală și animală"                             : "Biology"    ,
    "Chimie anorganică TEH Nivel I/II     "                    : "Chemistry"  ,
    "Fizică TEO"                                               : "Physics"    ,
    "Chimie anorganică TEO Nivel I/II"                         : "Chemistry"  ,
    "Filosofie"                                                : "Philosophy" ,
    "Fizică TEH"                                               : "Physics"    ,
    "Chimie organică TEH Nivel I/II"                           : "Chemistry"  ,
    "Informatică MI C/C++"                                     : "Informatics",
    "Informatică MI Pascal"                                    : "Informatics",
    "Logică, argumentare și comunicare"                        : "Logic"      ,
    "Sociologie"                                               : "Sociology"  ,
}

path_xlsx = DATA / "2023.07.19_bac_date-deschise_2023-ses1.xlsx"
path_csv = path_xlsx.with_suffix(".csv")
path_arrow = path_xlsx.with_suffix(".arrow")

# get dataset from here
# https://data.gov.ro/dataset/rezultate-bacalaureat-2023-sesiunea-1/resource/9635d473-edcb-4df6-af26-968f8030df54

if not path_csv.exists():
    Xlsx2csv(str(path_xlsx)).convert(str(path_csv))

if not path_arrow.exists():
    pl.read_csv(path_csv).write_ipc(path_arrow)

df_all = pl.read_ipc(path_arrow).lazy()

df_all = (
    df_all
    .with_columns(
        pl.any_horizontal(
            pl.all_horizontal(
                # "Fileira" :sigh:
                pl.col("Fileira").eq("Teoretică"),
                pl.col("Profil").eq("Real"),
            ),
            pl.all_horizontal(
                pl.col("Fileira").eq("Vocațională"),
                pl.col("Profil").eq("Militar"),
            ),
        ).alias("can_info"),
        pl.col("Subiect ed").map_dict(SUBJECT_MAP).alias("subject"),
    )
    .with_columns(
        pl.col("subject").eq("Informatics").alias("is_info"),
    )
    .collect()
)

# Double check that there are no people for whom can_info is False but is_info
# is True.
assert (
    len(
        df_all.filter(
            pl.col("is_info").and_(pl.col("can_info").not_())
        )
    ) == 0
)
# print(df_all.collect())

df_can_info: pl.DataFrame = df_all.filter(pl.col("can_info"))
# print(df_all.get_column("Subiect ed").unique().to_list())

total = len(df_all)
total_is_info = len(df_all.filter(pl.col("is_info")))

total_can_info = len(df_can_info)
total_can_info_is_info = len(df_can_info.filter(pl.col("is_info")))

print("total:", total)
print("total is info:", total_is_info, total_is_info / total)
print("total can info:", total_can_info)
print("total can info is info:", total_can_info_is_info, total_can_info_is_info / total_can_info)

print(df_can_info.group_by("subject").agg(pl.count()))
