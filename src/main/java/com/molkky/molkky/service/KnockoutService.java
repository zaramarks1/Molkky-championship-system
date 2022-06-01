package com.molkky.molkky.service;


import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.model.phase.PhaseRankingModel;
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

    @Autowired
    NotificationService notificationService;


    Map<Round, List<Match>> generateRounds(Knockout knockout) {
        Map<Round, List<Match>> results = new HashMap<>();

        List<Team> teamsOld = knockout.getTournament().getTeams();
        List<Team> teams;

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

    void validateRound(Round round){

        List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInRound(round, 2);

        int nbEliminated = scoresList.size()/2;

        for(int i = nbEliminated; i < scoresList.size();i++){
            scoresList.get(i).getTeam().setEliminated(true);
        }

        List<Team> teams = new ArrayList<>();

        if(Boolean.TRUE.equals(round.getPhase().getSeedingSystem())) {
            for (PhaseRankingModel p : scoresList) {
                Team team = p.getTeam();

                team.setNbPoints(team.getNbPoints() + p.getTotalPoints());

                teams.add(team);

            }
        }

        teams = teamRepository.saveAll(teams);

        generateNotificationAfterRound(teams);

    }

    void generateNotificationAfterRound(List<Team> teams){

        for(int i=0;i<teams.size();i++) {
            Team t = teams.get(i);
            String message ;
            if(t.isEliminated()){
                message = "Ton équipe a malheureuseusement été disqualifiée dans la phase de tableau éliminatoire";
            }else{
                message = " Felicitations! Ton équipe est qualifiée pour la prochaine phase de tableau éliminatoire";
            }

            notificationService.sendNotificationToList(message, "", t.getUserTournamentRoles());

        }
    }
}
