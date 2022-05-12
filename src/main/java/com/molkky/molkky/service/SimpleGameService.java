package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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

     HashMap<Round, List<Match>> generateRounds(SimpleGame simpleGame) {
        HashMap<Round, List<Match>> results = new HashMap();

        if(Boolean.FALSE.equals(simpleGame.getRanking())) {
            List<Team> teamsOld = simpleGame.getTournament().getTeams();

            List<Team> teamsNew = teamsOld.stream()
                    .filter(team -> !team.isEliminated())
                    .collect(Collectors.toList());

            List<Round> rounds = new ArrayList<>();
            List<Match> matches = new ArrayList<>();
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

                   // round = roundRepository.save(round);
                   // round = roundRepository.save(round);
                    //match =  matchRepository.save(match);
                    phaseRepository.save(simpleGame);
                    results.put(round, List.of(match));

                }
            }

               teamRepository.saveAll(teamsUpdated);

        }
        return results;
    }
}
