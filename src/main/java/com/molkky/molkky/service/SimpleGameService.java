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

            List<Team> teamsOld = simpleGame.getTournament().getTeams();
            List<Team> teams = new ArrayList<>();
            if(Boolean.FALSE.equals(simpleGame.getRanking()) || simpleGame.getNbPhase() == 1) {

                teams = teamsOld.stream()
                        .filter(team -> !team.isEliminated())
                        .collect(Collectors.toList());
            }

            List<Team> teamsUpdated = new ArrayList<>();

                for (int i = 0; i < teams.size()-1; i = i + 2) {

                    Team team1 = teams.get(i);
                    Team team2 = teams.get(i+1);

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

                    if((teams.size()-1 == i+2) && teams.size()%2 !=0){
                        Team team3 = teams.get(teams.size()-1);

                        Match match2 = new Match();
                        match.setRound(round);
                        match2.setTeams(List.of(team1, team3));
                        round.getMatches().add(match2);

                        Match match3 = new Match();
                        match.setRound(round);
                        match3.setTeams(List.of(team2, team3));
                        round.getMatches().add(match3);

                        team1.getMatchs().add(match2);
                        team2.getMatchs().add(match3);

                        team3.getMatchs().addAll(List.of(match2, match3));

                        teamsUpdated.add(team3);

                    }

                    teamsUpdated.add(team1);
                    teamsUpdated.add(team2);

                    simpleGame.getRounds().add(round);


            }

            simpleGame= phaseRepository.save(simpleGame);
            teamRepository.saveAll(teamsUpdated);

               for(Round r : simpleGame.getRounds()){
               results.put(r, r.getMatches());
              }


        return results;
    }

    void validateRound(Round round){

            List<Team> teamsEliminated = new ArrayList<>();
                if(round.getMatches().size() == 1){
                    Team winner = round.getMatches().get(0).getWinner();

                    for(Team t : round.getMatches().get(0).getTeams()){
                        if(!t.getId().equals(winner.getId())){
                            t.setEliminated(true);
                            teamsEliminated.add(t);
                        }
                    }
                }else{
                    // add if round has more than one match (3 teams)
                }
                
            teamRepository.saveAll(teamsEliminated);
    }
}
