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

import java.util.*;
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

    @Autowired
    RoundService roundService;

    Map<Round, List<Match>> generateRounds(Knockout knockout) {
        Map<Round, List<Match>> results = new HashMap<>();

        List<Team> teamsOld = knockout.getTournament().getTeams();
        List<Team> teams;

        //int nbMatch = knockout.getNbMatch();

        teams = teamsOld.stream()
                .filter(team -> !team.isEliminated())
                .collect(Collectors.toList());

        if(Boolean.TRUE.equals(knockout.getRanking()) ) {
            teams.sort(Comparator
                    .comparing(Team :: getNbPoints)
                    .reversed());
        }

        List<Team> teamsUpdated = new ArrayList<>();

        Round round = new Round();
        round.setPhase(knockout);
        round.setType(PhaseType.KNOCKOUT);
        round.setTeams(teams);

        for (int i = 0; i < teams.size()-1; i = i + 2) {
            List<Match> matches = new ArrayList<>();
            Team team1 = teams.get(i);
            Team team2 = teams.get(i+1);

            Match match = new Match();
            match.setRound(round);
            match.setTeams(List.of(team1, team2));
            matches.add(match);

            team1.getMatchs().add(match);
            team2.getMatchs().add(match);

            team1.getRounds().add(round);
            team2.getRounds().add(round);

            if((teams.size()-1 == i+2) && teams.size()%2 !=0){
                Team team3 = teams.get(teams.size()-1);

                Match match2 = new Match();
                match2.setRound(round);
                match2.setTeams(List.of(team1, team3));

                matches.add(match2);

                Match match3 = new Match();
                match3.setRound(round);
                match3.setTeams(List.of(team2, team3));
                matches.add(match3);

                team1.getMatchs().add(match2);
                team2.getMatchs().add(match3);

                team3.getMatchs().addAll(List.of(match2, match3));

                teamsUpdated.add(team3);

            }

            teamsUpdated.add(team1);
            teamsUpdated.add(team2);

            round.getMatches().addAll(roundService.createSetsFromMatch(matches));
        }

        knockout.getRounds().add(round);
        knockout= phaseRepository.save(knockout);
        teamRepository.saveAll(teamsUpdated);

        for(Round r : knockout.getRounds()){
            results.put(r, r.getMatches());
        }
        return results;
    }
}
