drop table if exists iwatchtheoffice_versions;
create table iwatchtheoffice_versions (
       id integer not null primary key autoincrement,
       started_at datetime not null,
       completed_at datetime null,
       status text CHECK(status IN ('pending', 'completed'))
);

drop table if exists iwatchtheoffice_seasons;

create table iwatchtheoffice_seasons (
       id integer not null primary key autoincrement,
       code integer not null,
       thumbnail text not null,
       href text not null
--       foreign key (version_id) references iwatchtheoffice_versions(id)
);
