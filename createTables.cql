CREATE KEYSPACE voting WITH replication = {'class': 'SimpleStrategy', 'replication_factor' : 1};

CREATE TABLE voting.pluralityl (
  voteId uuid,
  election text,
  candidate text,
  PRIMARY KEY (election, voteId)
);

CREATE TABLE voting.approval (
  voteId uuid,
  election text,
  candidate text,
  PRIMARY KEY (election, voteId)
);

CREATE TABLE voting.range (
  voteId uuid,
  election text,
  candidate text,
  amount int,
  PRIMARY KEY (election, voteId)
);
