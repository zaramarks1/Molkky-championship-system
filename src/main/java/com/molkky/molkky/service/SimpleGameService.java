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
    RoundService roundService;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    NotificationService notificationService;



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

                    roundService.createMatchSimpleAndKnockoutAndSwiss(teamsUpdated,  team1, team2, round);
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

      List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInPhase(round.getPhase(), 1);
        roundService.seedingSystem(round, scoresList);

        if(roundService.isPhaseOver(round.getPhase(), scoresList)){
            generateNotificationAfterRound(scoresList);
        }

    }


    public void generateNotificationAfterRound(List<PhaseRankingModel>  scoresList){

        for(int i=0;i<scoresList.size();i++) {
            Team t = scoresList.get(i).getTeam();
            String message ;
            if(t.isEliminated()){
                message = "Ton équipe a fini "+(i+1)+" et malheureuseusement été disqualifiée pendant la phase partie simple";
            }else{
                message = " Félicitations! Ton équipe a fini "+(i+1)+" et est qualifiée pour la prochaine phase";
            }

            notificationService.sendNotificationToList(message, "", t.getUserTournamentRoles());

        }
    }
}
