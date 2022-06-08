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
           return null;
       }

    }

    void validateRound(Round round){
        List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInPhase( round.getPhase(), round.getPhase().getVictoryValue());

        List<Team> teams = roundService.seedingSystem(round, scoresList);
        generateNotificationAfterRoundSwiss(teams);
        roundService.isPhaseOver(round.getPhase());

    }

    void generateNotificationAfterRoundSwiss(List<Team> teams){

        for (int i = 0; i <teams.size();i++) {
            String message = " Ton quipe a fini " +i+1+ "dans ce sub round de poule swiss";
            notificationService.sendNotificationToList(message, "", teams.get(i).getUserTournamentRoles());

        }
    }

}
