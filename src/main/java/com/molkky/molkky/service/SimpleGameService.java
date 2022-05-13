package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class SimpleGameService {

    @Autowired
    RoundRepository roundRepository;

    @Autowired
    MatchRepository matchRepository;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    PhaseRepository phaseRepository;

        Map<Round, List<Match>> generateRounds(SimpleGame simpleGame) {
        Map<Round, List<Match>> results = new HashMap<>();

        if(Boolean.FALSE.equals(simpleGame.getRanking())) {
            List<Team> teamsOld = simpleGame.getTournament().getTeams();

            List<Team> teamsNew = teamsOld.stream()
                    .filter(team -> !team.isEliminated())
                    .collect(Collectors.toList());

            List<Team> teamsUpdated = new ArrayList<>();
               if (teamsNew.size()%2 == 0) {
                for (int i = 0; i < teamsNew.size()-1; i = i + 2) {

                    Team team1 = teamsNew.get(i);
                    Team team2 = teamsNew.get(i+1);

                    Round round = new Round();
                    round.setPhase(simpleGame);
                    round.setType(PhaseType.SIMPLEGAME);
                    round.setTeams(List.of(team1, team2));

                    Match match = new Match();
                    match.setRound(round);
                    match.setTeams(List.of(team1, team2));
                    round.getMatches().add(match);


                    team1.getMatchs().add(match);
                    team2.getMatchs().add(match);

                    team1.getRounds().add(round);
                    team2.getRounds().add(round);


                    teamsUpdated.add(team1);
                    teamsUpdated.add(team2);

                    simpleGame.getRounds().add(round);

                   simpleGame= phaseRepository.save(simpleGame);

                }
            }

            teamRepository.saveAll(teamsUpdated);

               for(Round r : simpleGame.getRounds()){
               results.put(r, r.getMatches());
              }

        }
        return results;
    }
}
