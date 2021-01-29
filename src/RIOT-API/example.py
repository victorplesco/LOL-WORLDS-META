from lolesports_api import Lolesports_API
from time import sleep

api = Lolesports_API()
def handle_livematches():
    while True:
        live_matches = api.get_live()
        for live_match in live_matches['schedule']['events']:
            if live_match['state'] == 'inProgress':
                print(f"{live_match['match']['teams'][0]['code']} vs {live_match['match']['teams'][1]['code']}")
                get_live_game(api.get_event_details(live_match['id']))
                sleep(60)

def get_live_game(games):
    for game in games['event']['match']['games']:
        if game['state'] == 'inProgress':
            current_game = api.get_window(game['id'])
            print(current_game)

if __name__ == '__main__':
    handle_livematches()
