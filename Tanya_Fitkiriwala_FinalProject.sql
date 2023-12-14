create database pharmacy;
use pharmacy;

create table dim_member(
member_id varchar(100) not null,
member_first_name varchar(100),
member_last_name varchar(100),
member_birth_date date,
member_age int,
member_gender varchar(20)
);
ALTER TABLE dim_member add member_key int auto_increment primary key;

CREATE TABLE dim_drug(
    drug_ndc VARCHAR(100) NOT NULL,
    drug_name VARCHAR(100),
    drug_form_code VARCHAR(100),
    drug_form_desc VARCHAR(100),
    drug_brand_generic_code INT,
    drug_brand_generic_desc VARCHAR(100)
);
ALTER TABLE dim_drug add drug_key INT AUTO_INCREMENT PRIMARY KEY;

CREATE TABLE fact_prescription (
     member_key INT,
    drug_key INT,
    fill_date DATE,
    copay INT,
    insurance_paid INT
);

alter table fact_prescription add prescription_key INT AUTO_INCREMENT PRIMARY KEY;

alter table fact_prescription
add FOREIGN KEY (member_key) REFERENCES dim_member (member_key) ON DELETE CASCADE ON UPDATE CASCADE,
add FOREIGN KEY (drug_key) REFERENCES dim_drug (drug_key) ON DELETE CASCADE ON UPDATE CASCADE;



insert into dim_member values
(10001,'David','Dennison','1946-06-14',72,'M',1),
(10002,'John','Smith','1962-01-02',56,'M',2),
(10003,'Jane','Doe','1982-05-04',36,'F',3),
(10004,'Elaine','Rogers','1983-10-12',34,'F',4);

insert into dim_drug values
(433530848,'Risperidone','TB','Tablet',1,'Generic',1),
(545695193,'Amoxicillin','OS','Oral Solution',1,'Generic',2),
(545693828,'Ambien','TB','Tablet',2,'Brand',3),
(185085302,'Diprosone','TC','Topical Cream',1,'Generic',4);

insert into fact_prescription values
(1,1,'2017-10-31',15,50,1),
(2,2,'2018-06-14',50,130,2),
(3,3,'2017-12-30',35,250,3),
(4,4,'2017-11-09',15,600,4),
(1,3,'2018-01-15',20,650,5),
(1,1,'2018-02-22',15,48,6),
(3,3,'2018-05-16',35,322,7),
(4,4,'2017-12-08',15,712,8),
(1,3,'2018-02-14',20,648,9),
(1,1,'2018-05-08',15,55,10),
(1,3,'2018-03-13',20,648,11);


SELECT t2.drug_name, COUNT(*) AS prescription_count
FROM fact_prescription AS t1
JOIN dim_drug AS t2 ON t1.drug_key = t2.drug_key
GROUP BY drug_name;

SELECT
CASE WHEN member_age >= 65 THEN 'Age 65+'
ELSE '< 65'END AS age_group,
    COUNT(*) AS total_prescriptions,
    COUNT(DISTINCT t1.member_key) AS unique_members,
    SUM(copay) AS total_copay,
    SUM(insurance_paid) AS total_insurance_paid
FROM fact_prescription t1
join dim_member t2 on t1.member_key = t2.member_key
GROUP BY age_group;

WITH RankedPrescriptions AS (
    SELECT
        t2.member_id,t2.member_first_name,t2.member_last_name,t3.drug_name,t1.fill_date,t1.insurance_paid,
        RANK() OVER (PARTITION BY t2.member_id ORDER BY t1.fill_date DESC) AS ranks
    FROM fact_prescription t1
    JOIN dim_member t2 ON t1.member_key = t2.member_key
    JOIN dim_drug t3 ON t1.drug_key = t3.drug_key
)
SELECT
    member_id,
    member_first_name,
    member_last_name,
    drug_name,
    fill_date as most_recent_fill_date,
    insurance_paid as most_recent_insurance_paid
FROM RankedPrescriptions
WHERE ranks = 1;




select member_first_name, fill_date, insurance_paid from fact_prescription a
join dim_member b on a.member_key = b.member_key
order by fill_date desc;

select distinct * from dim_member;


