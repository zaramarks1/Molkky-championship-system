package com.molkky.molkky.service;


import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.rounds.Knockout;
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
public class KnockoutService {

    @Autowired
    RoundRepository roundRepository;

    @Autowired
    MatchRepository matchRepository;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    PhaseRepository phaseRepository;

    Map<Round, List<Match>> generateRounds(Knockout knockout) {
        Map<Round, List<Match>> results = new HashMap<>();

        List<Team> teamsOld = knockout.getTournament().getTeams();
        List<Team> teams = new ArrayList<>();

        int nbMatch = knockout.getNbMatch();

        if(Boolean.FALSE.equals(knockout.getRanking()) || knockout.getNbPhase() == 1) {

            teams = teamsOld.stream()
                    .filter(team -> !team.isEliminated())
                    .collect(Collectors.toList());
        }

        List<Team> teamsUpdated = new ArrayList<>();

        for (int i = 0; i < teams.size()-1; i = i + 2) {

            Team team1 = teams.get(i);
            Team team2 = teams.get(i+1);

            Round round = new Round();
            round.setPhase(knockout);
            round.setType(PhaseType.KNOCKOUT);
            round.setTeams(List.of(team1, team2));

            for(int j = 0; j < nbMatch;j++){
                Match match = new Match();
                match.setRound(round);
                match.setTeams(List.of(team1, team2));
                round.getMatches().add(match);

                team1.getMatchs().add(match);
                team2.getMatchs().add(match);

                team1.getRounds().add(round);
                team2.getRounds().add(round);
            }

            teamsUpdated.add(team1);
            teamsUpdated.add(team2);

            knockout.getRounds().add(round);

        }

        knockout= phaseRepository.save(knockout);
        teamRepository.saveAll(teamsUpdated);

        for(Round r : knockout.getRounds()){
            results.put(r, r.getMatches());
        }


        return results;
    }
}
