package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import java.util.*;
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

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    NotificationService notificationService;

    @Autowired
    RoundService roundService;

        Map<Round, List<Match>> generateRounds(SimpleGame simpleGame) {
            Map<Round, List<Match>> results = new HashMap<>();

            List<Team> teamsOld = simpleGame.getTournament().getTeams();
            List<Team> teams;

            teams = teamsOld.stream()
                    .filter(team -> !team.isEliminated())
                    .collect(Collectors.toList());

            if(Boolean.TRUE.equals(simpleGame.getRanking()) ) {
                teams.sort(Comparator
                        .comparing(Team :: getNbPoints)
                        .reversed());
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


        List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInRound(round, 2);
        scoresList.get(1).getTeam().setEliminated(true);

        List<Team> teams = new ArrayList<>();
        teams.add(scoresList.get(0).getTeam());
        teams.add(scoresList.get(1).getTeam());

            if(Boolean.TRUE.equals(round.getPhase().getSeedingSystem())){
                    teams.get(0).setNbPoints(teams.get(0).getNbPoints() + round.getMatches().get(0).getScoreTeam1());
                    teams.get(1).setNbPoints(teams.get(1).getNbPoints() + round.getMatches().get(0).getScoreTeam2());
            }

          teams = teamRepository.saveAll(teams);

        generateNotificationAfterRound(teams);

    }

    void generateNotificationAfterRound(List<Team> teams){

        for(int i=0;i<teams.size();i++) {
            Team t = teams.get(i);
            String message ;
            if(t.isEliminated()){
                message = "Ton equipe a été malheuseusement  dequalifié";
            }else{
                message = " Felicitations! Ton equipe est qualifié à la prochaine phase";
            }

            notificationService.sendNotificationToList(message, "", t.getUserTournamentRoles());

        }
    }
}
