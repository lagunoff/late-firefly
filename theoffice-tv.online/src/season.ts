import db, { Db, Season, Series } from "./db";
import jss from 'jss';
import { h, Jet, noop, array, ArrayItemModel } from "typescript-sdom";
import * as r from 'typescript-invertible-router';
import * as routes from './routes';

// Route
export const seasonAdapter = r.adapter.custom(applySeason, unapplySeason) as r.CustomAdapter<number, { nonEmpty }>;
export const parser = r.tag('Season').segment('season', seasonAdapter);
export type Route = typeof parser['_O'];

// Model
export type Model = {
  route: Route;
  season: Season;
};

// Action
export type Action = never;

// Init
export function init(db: Db, route: Route): Model {
  const season = db.seasons[route.season - 1];
  return { season, route: route };
}

// View
export function view(db: Db) {
  const href = (m: ArrayItemModel<Model['season']['series'], Model>) => routes.print({ tag: 'Episode', season: m.parent.route.season, episode: m.item.code });
  
  return h<Model, Action>('main', { class: classes.content }).childs(
    h.h1(m => m.season.code),
    //    h.img({ src: m => m.season.thumbnail }),
    h.ul({ class: classes.ul }).childs(
      array<Model, 'season', 'series'>('season', 'series')(
        h.li<ArrayItemModel<Model['season']['series'], Model>, Action>({ class: classes.li }).childs(
          h.a({ href }, (
            h.img({ class: 'rounded' + ' ' + classes.img, src: m => m.item.thumbnail })
          )),
          h.div(
            h.a({ href }, h.h3(m => 'Episode ' + m.item.code.replace(/^S\d\dE0?/, '') + ' - ' + m.item.name)),
            h.p(m => m.item.short_description),
          ),
        )
      )
    ),
  );
}

// Styles
function styles() {
  const unit = 8;
  const gridPadding = unit * 2;
  const columns = 3;
  const maxWidth = 840;
  
  return {
    content: {
      maxWidth: maxWidth + (gridPadding * (columns - 1)),
      margin: [0, 'auto'],
    },

    img: {
      width: 260,
    },

    ul: {
      padding: 0,
    },

    li: {
      listStyle: 'none',
      display: 'flex',
      margin: [unit * 2, 0],
      '& > * + *': { marginLeft: unit * 2 },
    },
  };
}

const { classes } = jss.createStyleSheet(styles()).attach();


// ----[ Helpers ]------


function applySeason(str: string) {
  const match = str.match(/^season-(\d+)$/); if (!match) return r.none;
  const numberOrNaN = Number(match[1]); if (isNaN(numberOrNaN)) return r.none;
  if (!db.seasons[numberOrNaN - 1]) return r.none;
  return r.some(numberOrNaN);
}

function unapplySeason(idx: number) {
  return 'season-' + idx;
}
