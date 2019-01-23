CREATE TABLE pictures (
    id serial PRIMARY KEY,
    file_path varchar (255) NOT NULL
);

CREATE TABLE users (
    id serial PRIMARY KEY,
    first_name varchar (255) NOT NULL,
    last_name varchar (255) NOT NULL,
    user_picture integer REFERENCES pictures,
    created_on timestamp NOT NULL,
    is_admin boolean DEFAULT false
);

CREATE TABLE authors (
    id serial PRIMARY KEY,
    user_id integer REFERENCES users,
    description text
);

CREATE TABLE news_categories (
    id serial PRIMARY KEY,
    name varchar (255) NOT NULL
);

CREATE TABLE news_objects (
    id serial PRIMARY KEY,
    title varchar (255) NOT NULL,
    created_on timestamp NOT NULL,
    author integer REFERENCES authors,
    category integer REFERENCES news_categories,
    content text,
    picture integer REFERENCES pictures,
    is_draft boolean DEFAULT true
);


CREATE TABLE tags (
    id serial PRIMARY KEY,
    name varchar (255) NOT NULL
);

CREATE TABLE news_tags (
    tag_id integer REFERENCES tags,
    news_id integer REFERENCES news_objects,
    PRIMARY KEY (tag_id, news_id)
);

CREATE TABLE news_comments (
    id serial PRIMARY KEY,
    news_id integer REFERENCES news_objects,
    user_id integer REFERENCES users,
    text text
);

CREATE TABLE news_pictures (
    picture_id integer REFERENCES pictures,
    news_id integer REFERENCES news_objects,
    PRIMARY KEY (picture_id, news_id)
);
