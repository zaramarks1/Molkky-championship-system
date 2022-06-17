package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.rounds.SwissPool;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.SwissPoolRepository;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class SwissService {


    @Autowired
    TeamRepository teamRepository;

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    NotificationService notificationService;

    @Autowired
    RoundService roundService;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    SwissPoolRepository swissPoolRepository;


    Map<Round, List<Match>> generateRounds(SwissPool swissPool) {

        int indexSubRound = swissPool.getIndexSubRound();
        if(swissPool.getNbSubRounds()>indexSubRound) {
           swissPool.setIndexSubRound(swissPool.getIndexSubRound()+1);
           swissPool = swissPoolRepository.save(swissPool);
           return roundService.generateRoundKnockoutAndSwiss(swissPool);
        }else {
           return new HashMap<>();
        }

    }

    void validateRound(Round round){

        SwissPool phase = (SwissPool) round.getPhase();
        List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInPhase( phase, phase.getVictoryValue());

        List<Team> teams = roundService.seedingSystem(round, scoresList);

        if(roundService.isPhaseOver(round.getPhase(), scoresList)){
            generateNotificationAfterPhaseSwiss(scoresList);
        }

        generateNotificationAfterRoundSwiss(teams);


    }

    void generateNotificationAfterRoundSwiss(List<Team> teams){

        for (int i = 0; i <teams.size();i++) {
            String message = " Ton quipe a fini " +i+1+ "dans ce sub round de poule swiss";
            notificationService.sendNotificationToList(message, "", teams.get(i).getUserTournamentRoles());

        }
    }
    public void generateNotificationAfterPhaseSwiss(List<PhaseRankingModel>  scoresList){

        for(int i=0;i<scoresList.size();i++) {
            Team t = scoresList.get(i).getTeam();
            String message ;
            if(t.isEliminated()){
                message = "Ton équipe a fini "+(i+1)+" et est malheureuseusement éliminée de la phase de poule Suisses";
            }else{
                message = " Félicitations! Ton équipe a fini "+(i+1)+" et est qualifiée pour la prochaine phase";
            }

            notificationService.sendNotificationToList(message, "", t.getUserTournamentRoles());

        }
    }


}
