import { Db, Season } from "./db";
import jss from 'jss';
import { h } from "typescript-sdom";
import * as routes from './routes';

// Model
export type Model = {
};

// Action
export type Action = never;

// Init
export function init(): Model {
  return {};
}

// View
export function view(db: Db) {

  function renderSeason(season: Season, idx: number) {
    return h.li({ class: classes.seasonLi }).childs(
      h('h3', season.code),
      h.a({ href: routes.print({ tag: 'Season', season: idx + 1 }) }).childs(
        h.img({ class: 'rounded', src: season.thumbnail })
      ),
      h.div({ class: classes.episodes }).childs(
        h('b', season.series.length + ' episodes: '),
        ...season.series.map(seria => h.span(h.a({ href: routes.print({ tag: 'Episode', season: idx + 1, episode: seria.code }) }).childs(seria.code.replace(/^S0\d/, '')), ' '))
      ),
    );
  }  
  
  return h<Model, Action>('main', { class: classes.content }).childs(
    h.ul({ class: classes.seasonsUl }).childs(
      ...db.seasons.map(renderSeason)
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

    seasonsUl: {
      padding: 0,
      display: 'flex',
      justifyContent: 'space-between',
      flexWrap: 'wrap',
    },
    
    seasonLi: {
      width: maxWidth / columns,
      listStyle: 'none',
      margin: [unit * 2, 0, 0, 0],
      '& img': { width: '100%' },
    },

    episodes: {
      fontSize: 14,
      maxHeight: 22,
      overflow: 'hidden',
    },
  };
}

const { classes } = jss.createStyleSheet(styles()).attach();
