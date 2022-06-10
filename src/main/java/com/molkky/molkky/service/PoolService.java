package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import java.util.*;


@Service
public class PoolService {

    @Autowired
    RoundRepository roundRepository;

    @Autowired
    MatchRepository matchRepository;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    NotificationRepository notificationRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    NotificationService notificationService;

    @Autowired
    RoundService roundService;


    public Map<Round, List<Match>> generateRounds(Pool pool){
        Map<Round, List<Match>> results = new HashMap<>();

        int nbPool = pool.getNbPools();

        List<Team> teams = roundService.getTeamsSorted(pool);

        List<Round> rounds = new ArrayList<>();

            for(int i =1; i <= nbPool;i++){
                Round round = new Round();
                round.setPhase(pool);

                round.setType(PhaseType.POOL);
                rounds.add(round);
                pool.getRounds().add(round);
            }


            int count =0;
            for(Team t : teams){
                t.getRounds().add( rounds.get(count));
                rounds.get(count).getTeams().add(t);
                count++;

                if(count == nbPool ){
                    count = 0;
                }
            }

            List<Match> matches;

            for (Round r : rounds){
              int nbMatch =  r.getTeams().size() -1;
              matches = new ArrayList<>();
              for(int i =0; i < nbMatch;i++){
                  Team team1 = r.getTeams().get(i);
                  for(int j=i+1;j<=nbMatch;j++){
                      Team team2 = r.getTeams().get(j);
                      Match match = new Match();
                      match.setRound(r);
                      match.getTeams().add(team1);
                      match.getTeams().add(team2);

                      team1.getMatchs().add(match);
                      team2.getMatchs().add(match);

                      matches.add(match);

                      teamRepository.save(team1);
                      teamRepository.save(team2);

                  }
              }
               r.getMatches().addAll(roundService.createSetsFromMatch(matches));

                if (Boolean.TRUE.equals(pool.getRandomStaff())) roundService.assignRandomStaffToMatch(matches, pool);

            }

        pool = phaseRepository.save(pool);


        for(Round r : pool.getRounds()){
            results.put(r, r.getMatches());
        }


        return results;
    }

    void validateRound(Round round){

        Pool pool = (Pool) round.getPhase();

        int victoryValue = pool.getVictoryValue();
        int nbNextRound = pool.getNbTeamsQualified()/pool.getNbPools();
        List<Team> teams = new ArrayList<>();

        List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInRound(round, victoryValue);

        for(int i=0;i<scoresList.size();i++) {
            Team team = scoresList.get(i).getTeam();
            if(Boolean.TRUE.equals(pool.getSeedingSystem())){
                team.setNbPoints(team.getNbPoints() + scoresList.get(i).getTotalPoints());
            }
            teams.add(team);
            if (i > nbNextRound - 1) {
                teams.get(i).setEliminated(true);
            }
        }

        teams = teamRepository.saveAll(teams);

        generateNotificationAfterRound(teams);

        roundService.isPhaseOver(pool, scoresList);

    }


    void generateNotificationAfterRound(List<Team> teams){

        for(int i=0;i<teams.size();i++) {
            Team t = teams.get(i);
            String message ;
            if(t.isEliminated()){
                message = "Ton équipe a terminé " + (i + 1)+ " ème de sa poule et est malheuseusement éliminée.";
            }else{
                message = "Félicitations! Ton équipe a terminé" + (i + 1)+ " ème dans sa poule. Vous êtes qualifiés pour la prochaine phase.";
            }

            notificationService.sendNotificationToList(message, "", t.getUserTournamentRoles());

        }
    }
}
