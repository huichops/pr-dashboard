import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'), {
  githubToken: process.env.ELM_APP_GITHUB_TOKEN
});

registerServiceWorker();
