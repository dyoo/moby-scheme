-- PostgreSQL schema for j2me-world

create table owner (
  id serial,
  email text not null,
  primary key(id),
  unique (email)
);


create table project (
  id serial,
  name text not null,
  is_validated boolean not null,
  owner_id integer not null,
  primary key (id),
  foreign key (owner_id) references owner(id));


create table source (
  id serial,
  date timestamp not null,
  version integer not null,
  code bytea not null,
  project_id integer not null,
  primary key (id),
  foreign key (project_id) references project(id)
);


create table compiler (
  id serial,
  name text not null,
  version integer not null,
  primary key (id),
  unique(name, version)
);


create table compilation (
  id serial,
  type text not null,
  compiler_id integer not null,
  date_started timestamp not null,
  date_finished timestamp not null,
  source_id integer not null,
  result bytea,
  error text,
  primary key (id),
  foreign key (compiler_id) references compiler(id),
  foreign key (source_id) references source(id),
  check (type in ('binary', 'error'))
);


create table download (
  id serial,
  date timestamp not null,
  ip text not null,
  compilation_id integer not null,
  primary key (id),
  foreign key (compilation_id) references compilation(id)
);
