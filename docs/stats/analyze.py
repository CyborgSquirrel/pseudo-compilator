import dataclasses
import functools
import logging
import os.path
import pathlib
from urllib.parse import urlparse

import polars as pl
import pyexcel
import requests
from xlsx2csv import Xlsx2csv

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)

DATA = pathlib.Path("data")


@dataclasses.dataclass
class Dataset:
    year: int
    url: str

    @property
    def cache_dir(self):
        dir = DATA / str(self.year)
        dir.mkdir(parents=True, exist_ok=True)
        return dir

    @property
    def source_path(self):
        return self.cache_dir / os.path.basename(self.parsed_url.path)
    
    @functools.cached_property
    def parsed_url(self):
        return urlparse(self.url)
    
    def download(self) -> pathlib.Path:
        if self.source_path.exists():
            return self.source_path
        logger.info("downloading %s", self.url)
        request = requests.get(self.url)

        with self.source_path.open("wb") as f:
            f.write(request.content)
        return self.source_path

    @property
    def arrow_path(self) -> pathlib.Path:
        source_path = self.download()
        if source_path.suffix == ".xlsx":
            new_source_path = source_path.with_suffix(".csv")
            if not new_source_path.exists():
                Xlsx2csv(str(source_path)).convert(str(new_source_path))
            source_path = new_source_path

        if source_path.suffix == ".ods":
            new_source_path = source_path.with_suffix(".csv")
            if not new_source_path.exists():
                records = pyexcel.get_records(file_name=str(source_path))
                pyexcel.save_as(array=records, dest_file_name=str(new_source_path))
            source_path = new_source_path

        if source_path.suffix == ".csv":
            new_source_path = source_path.with_suffix(".arrow")
            if not new_source_path.exists():
                pl.read_csv(source_path).write_ipc(new_source_path)
            source_path = new_source_path

        if source_path.suffix != ".arrow":
            raise ValueError(f"unknown extension {source_path.suffix}")

        return source_path

    @functools.cached_property
    def df(self) -> pl.DataFrame:
        return pl.read_ipc(self.arrow_path)


DATASETS = [
    # This one appears to be broken.
    # Dataset(2014, "http://date.edu.ro/sites/default/files/BacInscriere2014_sesiunea_I_0_0.csv"),

    # Dataset(2015, "https://data.gov.ro/dataset/3d6d423b-f54f-43c3-ab4c-54f704a36ee5/resource/e2f3ecdd-e615-45ce-808f-9977dab7a453/download/bacinscriere2015sesiuneai00.csv"),
    Dataset(2015, "https://data.gov.ro/dataset/3d6d423b-f54f-43c3-ab4c-54f704a36ee5/resource/6a70b531-55d6-4389-88a1-b2496cf9dfa8/download/bacinscriere2015sesiuneai000.ods"),
    Dataset(2016, "https://data.gov.ro/dataset/a7f55a35-c114-4309-a048-88c4924024eb/resource/2e34e2ad-d082-4ef3-aeea-bc6437c72096/download/2016sesiuneai.csv"),
    Dataset(2017, "https://data.gov.ro/dataset/cb54fa0b-4d8c-4cef-b0d9-fe80e0c99743/resource/2b8d2567-2633-422a-a98b-4fb6cd9c0b09/download/2017-09-25-date-deschise-2017-i.csv"),
    Dataset(2018, "https://data.gov.ro/dataset/1007a44d-0b53-477a-bc49-8c59e00db39e/resource/bda23092-c00e-4feb-92bf-9df53e19f366/download/date-deschise-bac-2018-sesiunea-1.xlsx"),
    Dataset(2019, "https://data.gov.ro/dataset/83ab8216-a862-407c-ad74-7dba39d22061/resource/3ea1d1d9-4dc8-4f85-a761-815730b33c15/download/2019-08-06-date-deschise-bac-2019-i.csv"),
    Dataset(2020, "https://data.gov.ro/dataset/e996dc5c-48d1-4cbc-9aaf-2f4c3a2362a5/resource/b0de486e-fa2f-4380-9c6c-71e36d6e35c1/download/date-deschise-bac-2020-sesiunea-1.xlsx"),
    Dataset(2021, "https://data.gov.ro/dataset/6827d28b-76de-41e1-a75a-a9251b04714a/resource/2db91f1c-77a6-44bd-a5d7-e1c384c49275/download/2021.08.10_bac_date-deschise_2021.xlsx"),
    Dataset(2022, "https://data.gov.ro/dataset/0778231d-be65-41c8-9530-6f8dbceaaa08/resource/9e0419b2-342c-4849-ad69-78f6dab8efc4/download/2022.07.06_bac_export-sesiunea-1-2022.xlsx"),
    Dataset(2023, "https://data.gov.ro/dataset/90cd5404-01b7-4002-ac63-6e44917afbf9/resource/9635d473-edcb-4df6-af26-968f8030df54/download/2023.07.19_bac_date-deschise_2023-ses1.xlsx"),
]

YEAR_TO_DATASET = { dataset.year: dataset for dataset in DATASETS }

print(YEAR_TO_DATASET[2015].df)
exit()


SUBJECT_MAP = {
    "Chimie organică TEO Nivel I/II"                           : "Chemistry"       ,
    "Anatomie și fiziologie umană, genetică și ecologie umană" : "Biology"         ,
    "Geografie"                                                : "Geography"       ,
    "Economie"                                                 : "Economy"         ,
    "Psihologie"                                               : "Psychology"      ,
    "Informatică SN Pascal"                                    : "Computer Science",
    "Informatică SN C/C++"                                     : "Computer Science",
    "Biologie vegetală și animală"                             : "Biology"         ,
    "Chimie anorganică TEH Nivel I/II     "                    : "Chemistry"       ,
    "Fizică TEO"                                               : "Physics"         ,
    "Chimie anorganică TEO Nivel I/II"                         : "Chemistry"       ,
    "Filosofie"                                                : "Philosophy"      ,
    "Fizică TEH"                                               : "Physics"         ,
    "Chimie organică TEH Nivel I/II"                           : "Chemistry"       ,
    "Informatică MI C/C++"                                     : "Computer Science",
    "Informatică MI Pascal"                                    : "Computer Science",
    "Logică, argumentare și comunicare"                        : "Logic"           ,
    "Sociologie"                                               : "Sociology"       ,
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
