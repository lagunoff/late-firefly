export type Series = {
  code: string;
  name: string;
  href: string;
  short_description: string;
  thumbnail: string;
  description: string;
  links: string;
};

export type Season = {
  code: string;
  thumbnail: string;
  href: string;
  series: Series[];
};

export type Db = {
  seasons: Season[];
};

export default require('../db.json') as Db;

declare const require: any;
