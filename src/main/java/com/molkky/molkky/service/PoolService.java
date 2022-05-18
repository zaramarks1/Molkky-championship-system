package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.model.phase.PoolRankingModel;
import com.molkky.molkky.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import java.util.*;
import java.util.stream.*;

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

    public Map<Round, List<Match>> generateRounds(Pool pool){
        Map<Round, List<Match>> results = new HashMap();

        List<Team> teamsOld = pool.getTournament().getTeams();
        List<Team> teams = new ArrayList<>();
        int nbPool = pool.getNbPools();
        if(Boolean.FALSE.equals(pool.getRanking()) || pool.getNbPhase() == 1) {

             teams = teamsOld.stream()
                    .filter(team -> !team.isEliminated())
                    .collect(Collectors.toList());
        }

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
               r.getMatches().addAll(matches);

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
        List<PoolRankingModel> scoresList = new ArrayList<>();
        Map<Team, PoolRankingModel> scores = new HashMap<>();

        for(Team t : round.getTeams()){
            PoolRankingModel poolRankingModel = new PoolRankingModel();
            poolRankingModel.setTeam(t);
            scores.put(t, poolRankingModel);
        }

        for(Match m : round.getMatches()){
            Team team1 = m.getTeams().get(0);
            Team team2 = m.getTeams().get(1);

            PoolRankingModel poolRankingModel1 = scores.get(team1);
            poolRankingModel1.setTeam(team1);
            poolRankingModel1.setTotalPoints(poolRankingModel1.getTotalPoints() + m.getScoreTeam1());
            PoolRankingModel poolRankingModel2 = scores.get(team2);
            poolRankingModel2.setTeam(team2);
            poolRankingModel2.setTotalPoints(poolRankingModel2.getTotalPoints() + m.getScoreTeam2());

            for(Team t : m.getTeams()){
                if(t.getId().equals(team1.getId()) && t.getId().equals(m.getWinner().getId())){
                    poolRankingModel1.setValues(poolRankingModel1.getValues() + victoryValue);
                }else  if(t.getId().equals(team2.getId()) && t.getId().equals(m.getWinner().getId())){
                    poolRankingModel2.setValues(poolRankingModel2.getValues() + victoryValue);
                }
            }


            scores.put(team1,poolRankingModel1);
            scores.put(team2, poolRankingModel2);
        }

        List<Team> teams = new ArrayList<>();
        for(Map.Entry<Team, PoolRankingModel> entry : scores.entrySet()){

           Team team = entry.getKey();
           PoolRankingModel poolRankingModel= scores.get(team);

            scoresList.add(poolRankingModel);


        }

        scoresList.sort(Comparator
                .comparing(PoolRankingModel::getValues)
                .thenComparing(PoolRankingModel::getTotalPoints)
                .reversed());

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

        generateNotificationWinners(teams);


    }


    void generateNotificationWinners(List<Team> teams){

        for(int i=0;i<teams.size();i++) {
            Team t = teams.get(i);
            String message ;
            if(t.isEliminated()){
                message = "Ton equipe a fini " + (i + 1)+ " dans la poule et malheuseusement vous etes dequalifies";
            }else{
                message = " Felicitations! Ton equipe a fini " + (i + 1)+ " dans la poule et  vous etes dualifies à la prochaine phase";
            }

            for(UserTournamentRole userTournamentRole: t.getUserTournamentRoles()){
                Notification notification = new Notification();
                notification.setMessage(message);
                notification.setUser(userTournamentRole.getUser());
                notification.setUserTournamentRole(userTournamentRole);

                userTournamentRole.getNotifications().add(notification);
                userTournamentRole.getUser().getNotifications().add(notification);

                userTournamentRoleRepository.save(userTournamentRole);
            }

        }
    }
}
