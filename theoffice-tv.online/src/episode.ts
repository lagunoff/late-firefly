import { Db, Season, Series } from "./db";
import jss from 'jss';
import { h, Jet, noop, array, RawPatch, custom, applyPatch } from "typescript-sdom";
import * as r from 'typescript-invertible-router';
import * as routes from './routes';
import { seasonAdapter } from "./season";

// Route
export const parser = r.tag('Episode').segment('season', seasonAdapter).segment('episode', r.nestring);
export type Route = typeof parser['_O'];

// Model
export type Model = {
  route: Route;
  season: Season;
  episode: Series;
  active: number;
};

// Action
export type Action =
  | { tag: 'SourceClicked', link: string }
;

// Init
export function init(db: Db, route: Route): Model {
  const season = db.seasons[route.season - 1];
  const episode = db.seasons[route.season - 1].series.find(e => e.code === route.episode)!;
  return { season, route: route, episode, active: 3 };
}

// Update
export function update(action: Action, model: Model): RawPatch<Model> {
  switch (action.tag) {
    case 'SourceClicked': {
      const active = model.episode.links.indexOf(action.link);
      return { $patch: { active } };
    }
  }
}


// View
export function view(db: Db) {
  return h<Model, Action>('main', { class: classes.content }).childs(
    h.h1(m => 'Episode ' + m.episode.code.replace(/^S\d\dE0?/, '') + ' - ' + m.episode.name),
    h.ul({ class: `nav nav-tabs ${classes.tabs}` }).childs(
      array<Model, 'episode', 'links'>('episode', 'links')(
        h.li<any>({ class: 'nav-item' }).childs(
          h.a<any>({ href: m => m.item, class: m => 'nav-link' + (m.parent.episode.links.indexOf(m.item) === m.parent.active ? ' active' : '') }, m => new URL(m.item).host).on({
            click: (e, m) => (e.preventDefault(), { tag: 'SourceClicked', link: m.item })
        })),
      ),
    ),
    custom(createIframe, actuateIframe),
    h.p(m => m.episode.description),
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

    iframe: {
      display: 'block',
      width: maxWidth,
      height: maxWidth * 9 / 16,
    },

    tabs: {
      marginBottom: unit,
    },
  };
}

const { classes } = jss.createStyleSheet(styles()).attach();


function createIframe(model: Model) {
  const iframe = document.createElement('iframe');
  iframe.setAttribute('referrerpolicy', "no-referrer");
  iframe.setAttribute('src', model.episode.links[model.active]);
  iframe.setAttribute('class', classes.iframe + ' rounded');
  iframe.setAttribute('scrolling', 'no');
  iframe.setAttribute('allowfullscreen', 'true');
  iframe.setAttribute('frameborder', '0');
  return iframe;
}

function actuateIframe(el: any, jet: Jet<Model>) {
  if (jet._velocity.tag === 'key' && jet._velocity.key === 'active') {
    return createIframe(applyPatch(jet._position, jet._velocity));
  }
  return el;
}
