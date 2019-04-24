import * as r from 'typescript-invertible-router';
import * as season from './season';
import * as episode from './episode';

export const parser = r.oneOf(
  r.tag('Home'),
  season.parser,
  episode.parser,
);
export type Route = typeof parser['_O'];


export function print(route: Route) {
  return '#' + parser.print(route);
}

export function parse(location: Location) {
  const hash = location.hash && location.hash[0] === '#' ? location.hash.slice(1) : location.hash;
  return parser.parse(hash);
}
