CREATE TABLE IF NOT EXISTS author (
    id bigint auto_increment,
    name varchar (255),
    PARAM json,
    primary key (id)
);
CREATE TABLE IF NOT EXISTS book (
    id bigint auto_increment,
    name varchar (255),
    author_id bigint,
    foreign key (author_id) references author(id),
    primary key (id)
);