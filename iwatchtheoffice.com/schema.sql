drop table if exists iwatchtheoffice_versions;
create table iwatchtheoffice_versions (
       id int not null primary key,
       started_at datetime not null default now(),
       completed_at datetime null,
       status enum('pending', 'completed', 'failed') default 'pending'
);

drop table if exists iwatchtheoffice_seasons;
create table iwatchtheoffice_seasons (
       id int not null primary key,
       version_id int not null,
       number text not null,
       thumbnail text null,
       foreign key (version_id) references iwatchtheoffice_versions(id)
);
