package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import java.util.*;

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

            List<Team> teams = roundService.getTeamsSorted(simpleGame);

            List<Team> teamsUpdated = new ArrayList<>();

                for (int i = 0; i < teams.size()-1; i = i + 2) {
                    Team team1 = teams.get(i);
                    Team team2 = teams.get(i+1);

                    Round round = new Round();
                    round.setPhase(simpleGame);
                    round.setType(PhaseType.SIMPLEGAME);
                    round.setTeams(List.of(team1, team2));

                    roundService.createMatchSimpleAndKnockout(teamsUpdated,  team1, team2, round);
                    simpleGame.getRounds().add(round);
            }

            simpleGame= phaseRepository.save(simpleGame);
            teamRepository.saveAll(teamsUpdated);

               for(Round r : simpleGame.getRounds()){
               results.put(r, r.getMatches());
              }


        return results;
    }



    public void validateRound(Round round){

        List<Team> teams = round.getTeams();

            if(Boolean.TRUE.equals(round.getPhase().getSeedingSystem())){
                teams.get(0).setNbPoints(teams.get(0).getNbPoints() + round.getMatches().get(0).getScoreTeam1());
                teams.get(1).setNbPoints(teams.get(1).getNbPoints() + round.getMatches().get(0).getScoreTeam2());

                teamRepository.saveAll(teams);
            }

      phaseOver(round);

    }

    public void  phaseOver(Round round){
        SimpleGame simpleGame = (SimpleGame) round.getPhase();
        List<Team> teams = new ArrayList<>();

        if(Boolean.TRUE.equals(roundService.isPhaseOver(simpleGame))){
            List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInPhase(simpleGame, 1);
            int nbEliminated = scoresList.size() - simpleGame.getNbTeamsQualified();

            for(int i = 0; i < scoresList.size();i++){
                if (i >= nbEliminated) scoresList.get(i).getTeam().setEliminated(true);
                teams.add(scoresList.get(i).getTeam());
            }
            teamRepository.saveAll(teams);
            generateNotificationAfterRound(teams);
        }
    }

    public void generateNotificationAfterRound(List<Team> teams){

        for(int i=0;i<teams.size();i++) {
            Team t = teams.get(i);
            String message ;
            if(t.isEliminated()){
                message = "Ton équipe a malheureuseusement été disqualifiée";
            }else{
                message = " Felicitations! Ton équipe est qualifiée pour la prochaine phase";
            }

            notificationService.sendNotificationToList(message, "", t.getUserTournamentRoles());

        }
    }
}
