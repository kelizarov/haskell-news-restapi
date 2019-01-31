CREATE TABLE pictures (
    id serial PRIMARY KEY,
    created_on timestamp with time zone NOT NULL,
    file_path varchar (255) NOT NULL
);

CREATE TABLE users (
    id serial PRIMARY KEY,
    first_name varchar (255) NOT NULL,
    last_name varchar (255) NOT NULL,
    user_picture integer REFERENCES pictures,
    created_on timestamp with time zone NOT NULL,
    is_admin boolean DEFAULT false
);

CREATE TABLE authors (
    id serial PRIMARY KEY,
    user_id integer REFERENCES users,
    description text
);

CREATE TABLE categories (
    id serial PRIMARY KEY,
    name varchar (255) NOT NULL
);

CREATE TABLE posts (
    id serial PRIMARY KEY,
    title varchar (255) NOT NULL,
    created_on timestamp with time zone NOT NULL,
    author integer REFERENCES authors,
    category integer REFERENCES categories,
    content text,
    picture integer REFERENCES pictures,
    is_draft boolean DEFAULT true
);


CREATE TABLE tags (
    id serial PRIMARY KEY,
    name varchar (255) NOT NULL
);

CREATE TABLE post_tags (
    tag_id integer REFERENCES tags,
    post_id integer REFERENCES posts,
    PRIMARY KEY (tag_id, news_id)
);

CREATE TABLE comments (
    id serial PRIMARY KEY,
    created_on timestamp with time zone NOT NULL,
    user_id integer REFERENCES users,
    text text
);

CREATE TABLE post_comments (
    comment_id integer REFERENCES comments,
    post_id integer REFERENCES posts
    PRIMARY KEY (comment_id, post_id)
);

CREATE TABLE post_pictures (
    picture_id integer REFERENCES pictures,
    post_id integer REFERENCES posts,
    PRIMARY KEY (picture_id, news_id)
);
