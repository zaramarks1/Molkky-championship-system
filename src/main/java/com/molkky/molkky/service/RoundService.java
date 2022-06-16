package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.domain.rounds.SwissPool;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;
import type.TournamentStatus;
import type.UserRole;

import java.security.SecureRandom;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class RoundService {

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    MatchRepository matchRepository;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    MatchService matchService;

    private final SecureRandom rand = new SecureRandom();

    public List<PhaseRankingModel> orderTeamsByScoreInRound(Round round, int victoryValue){
        Map<Integer, PhaseRankingModel> scores = new HashMap<>();
        List<PhaseRankingModel> scoresList = new ArrayList<>();

        for(Team t : round.getTeams()){
            PhaseRankingModel phaseRankingModel = new PhaseRankingModel();
            phaseRankingModel.setTeam(t);
            scores.put(t.getId(), phaseRankingModel);
        }

        for(Match m : round.getMatches()){
            Team team1 = m.getTeams().get(0);
            Team team2 = m.getTeams().get(1);

            PhaseRankingModel phaseRankingModel1 = scores.get(team1.getId());
            phaseRankingModel1.setTeam(team1);
            phaseRankingModel1.setTotalPoints(phaseRankingModel1.getTotalPoints() + m.getScoreTeam1());
            PhaseRankingModel phaseRankingModel2 = scores.get(team2.getId());
            phaseRankingModel2.setTeam(team2);
            phaseRankingModel2.setTotalPoints(phaseRankingModel2.getTotalPoints() + m.getScoreTeam2());

            for(Team t : m.getTeams()){
                if(m.getWinner()!= null){
                    if(t.getId().equals(team1.getId()) && t.getId().equals(m.getWinner().getId())){
                        phaseRankingModel1.setValues(phaseRankingModel1.getValues() + victoryValue);
                    }else  if(t.getId().equals(team2.getId()) && t.getId().equals(m.getWinner().getId())){
                        phaseRankingModel2.setValues(phaseRankingModel2.getValues() + victoryValue);
                    }
                }
            }


            scores.put(team1.getId(), phaseRankingModel1);
            scores.put(team2.getId(), phaseRankingModel2);
        }

        for(Map.Entry<Integer, PhaseRankingModel> entry : scores.entrySet()){

            Team team = teamRepository.findById(entry.getKey());
            PhaseRankingModel phaseRankingModel = scores.get(team.getId());

            scoresList.add(phaseRankingModel);

        }

        scoresList.sort(Comparator
                .comparing(PhaseRankingModel::getValues)
                .thenComparing(PhaseRankingModel::getTotalPoints)
                .reversed());

        return scoresList;

    }

    public List<PhaseRankingModel> orderTeamsByScoreInPhase(Phase phase, int victoryValue){
        List<PhaseRankingModel> scoresList = new ArrayList<>();

        for(Round round : phase.getRounds()){
            scoresList.addAll(orderTeamsByScoreInRound( round, victoryValue));
        }

        if(phase instanceof SwissPool || phase instanceof Knockout){

            Map<Team, PhaseRankingModel> map = new HashMap<>();
            for(PhaseRankingModel p : scoresList){
                map.merge(p.getTeam(), p, (oldValue, newValue) -> new PhaseRankingModel(p.getTeam(),
                        oldValue.getTotalPoints() + newValue.getTotalPoints(),
                        oldValue.getValues() + newValue.getValues()));
            }

            List<PhaseRankingModel> scoresListNew = new ArrayList<>(map.values());
            scoresListNew.sort(Comparator
                    .comparing(PhaseRankingModel::getValues)
                    .thenComparing(PhaseRankingModel::getTotalPoints)
                    .reversed());

            return scoresListNew;
        }else{
            scoresList.sort(Comparator
                    .comparing(PhaseRankingModel::getValues)
                    .thenComparing(PhaseRankingModel::getTotalPoints)
                    .reversed());
            return scoresList;
        }

    }

    public List<Match> createSetsFromMatch(List<Match> matches){
        int nbSets = matches.get(0).getRound().getPhase().getNbSets();
        List<Match> results  = new ArrayList<>();

        for(Match m : matches){
            List<com.molkky.molkky.domain.Set> sets = new ArrayList<>();
            for(int i =0; i <nbSets; i++){
                Set set = new Set();
                set.setTeams(m.getTeams());
                set.setMatch(m);
                sets.add(set);
            }
            m.setSets(sets);
            results.add(m);
        }
        return results;
    }



    List<Team> getTeamsSorted(Phase phase){

        List<Team> teamsOld = phase.getTournament().getTeams();
        List<Team> teams;

        teams = teamsOld.stream()
                .filter(team -> !team.isEliminated())
                .collect(Collectors.toList());

        if(Boolean.TRUE.equals(phase.getRanking()) ) {
            teams.sort(Comparator
                    .comparing(Team :: getNbPoints)
                    .reversed());
        }

        //changer pour eviter affrontation d'un meme club
        if(Boolean.TRUE.equals(phase.getTerrainAffectation())){
          teams = sortByClub(teams);
        }

        return teams;
    }

    public List<Team> sortByClub(List<Team> teams){
        Map<String, Integer> clubs = new HashMap<>();

        for(Team t : teams){
            //t.getClub().getName()
            clubs.merge(t.getName(), 1, (oldValue, newValue) ->oldValue + newValue);
        }

        List<Team> teamsClub = new ArrayList<>();

        boolean finished = false;
        do {
            int idTeam1 = rand.nextInt(teams.size());
            int idTeam2 = rand.nextInt(teams.size());
            Team t1 = teams.get(idTeam1);
            Team t2 = teams.get(idTeam2);

            if(!t1.getName().equals(t2.getName())){
                teamsClub.addAll(List.of(t1, t2));
                teams.remove(t1);
                teams.remove(t2);

                clubs.put(t1.getName(), clubs.get(t1.getName())-1);
                clubs.put(t2.getName(), clubs.get(t2.getName())-1);

                if(clubs.get(t1.getName() )== 0) clubs.remove(t1.getName());
                if(clubs.get(t2.getName()) == 0) clubs.remove(t2.getName());

                if(clubs.size() == 1) {
                    finished = true;

                    teamsClub.addAll(teams);

                }

            }
        }while(finished);

        return teamsClub;
    }

    public  void createMatchSimpleAndKnockoutAndSwiss(List<Team> teamsUpdated, Team team1, Team team2, Round round) {
        Match match = new Match();
        match.setRound(round);
        match.setTeams(List.of(team1, team2));

        this.assignRandomStaffToMatch(List.of(match), round.getPhase());

        team1.getMatchs().add(match);
        team2.getMatchs().add(match);

        team1.getRounds().add(round);
        team2.getRounds().add(round);

        teamsUpdated.add(team1);
        teamsUpdated.add(team2);


        round.getMatches().addAll(this.createSetsFromMatch(List.of(match)));



    }
    
    public Map<Round, List<Match>> generateRoundKnockoutAndSwiss(Phase phase) {

        Map<Round, List<Match>> results = new HashMap<>();

        List<Team> teams = this.getTeamsSorted(phase);


        List<Team> teamsUpdated = new ArrayList<>();

        Round round = new Round();
        if (phase instanceof Knockout) {

            phase.setNbTeamsQualified(teams.size()/2);
            round.setPhase(phase);
            round.setType(PhaseType.KNOCKOUT);

        } else if (phase instanceof SwissPool) {
            round.setPhase(phase);
            round.setType(PhaseType.SWISSPOOL);

            int indexSubRound = ((SwissPool) phase).getIndexSubRound();
            if (indexSubRound != 1) {
                List<PhaseRankingModel> ranking = this.orderTeamsByScoreInPhase(phase, phase.getVictoryValue());

                teams = new ArrayList<>();

                for (PhaseRankingModel p : ranking) {
                    teams.add(p.getTeam());
                }
            }
        }

        round.setTeams(teams);


        for (int i = 0; i < teams.size() - 1; i = i + 2) {
            Team team1 = teams.get(i);
            Team team2 = teams.get(i + 1);

            this.createMatchSimpleAndKnockoutAndSwiss(teamsUpdated, team1, team2, round);
        }

        phase.getRounds().add(round);
        phase = phaseRepository.save(phase);
        teamRepository.saveAll(teamsUpdated);

        for (Round r : phase.getRounds()) {
            results.put(r, r.getMatches());
        }

        return results;
    }

    List<Team> seedingSystem(Round round, List<PhaseRankingModel>  scoresList){

        List<Team> teams = new ArrayList<>();

            for (PhaseRankingModel p : scoresList) {
                Team team = p.getTeam();

                if(Boolean.TRUE.equals(round.getPhase().getSeedingSystem())) {
                    team.setNbPoints(team.getNbPoints() + p.getTotalPoints());
                }

                teams.add(team);
            }

        return teamRepository.saveAll(teams);
    }

    public boolean isPhaseOver(Phase phase, List<PhaseRankingModel>  scoresList){

        boolean response = true;
        for(Round r: phase.getRounds()){

            if(Boolean.FALSE.equals(r.getFinished())) return false;
        }

        if (phase instanceof Knockout){
            List<Team> teams = phase.getTournament().getTeams().stream()
                    .filter(team -> !team.isEliminated())
                    .collect(Collectors.toList());
            if(teams.size() == 1) {
                phase.setFinished(true);
                phaseRepository.save(phase);

            }else return false;
        }else if (phase instanceof SwissPool) {
            SwissPool s = (SwissPool) phase;
            if (Objects.equals(s.getIndexSubRound(), s.getNbSubRounds())) {
                phaseOverAction(phase, scoresList);
                phase.setFinished(true);
                phaseRepository.save(phase);

            } else return false;
            }else if (phase instanceof SimpleGame){
                phaseOverAction(phase, scoresList);
                phase.setFinished(true);
                phaseRepository.save(phase);

        }else{
            phase.setFinished(true);
            phaseRepository.save(phase);
        }

        isTournamentOver(phase.getTournament());

        return response;

    }

    public void  phaseOverAction(Phase phase, List<PhaseRankingModel>  scoresList){

        List<Team> teams = new ArrayList<>();
        int nbEliminated = phase.getNbTeamsQualified();

        for(int i = nbEliminated; i < scoresList.size();i++){
            scoresList.get(i).getTeam().setEliminated(true);
        }
            teamRepository.saveAll(teams);

    }

    public boolean isTournamentOver(Tournament tournament){
        for(Phase p: tournament.getPhases()){
            if(Boolean.FALSE.equals(p.getFinished())) return false;
        }
        tournament.setStatus(TournamentStatus.ENDED);
        tournament.setFinished(true);
        tournamentRepository.save(tournament);

        return true;
    }

    public void assignRandomStaffToMatch(List<Match> matches, Phase phase){


        if(Boolean.TRUE.equals(phase.getRandomStaff())){

            List<UserTournamentRole> users = phase.getTournament().getUserTournamentRoles();
            List<UserTournamentRole> staffs;
            staffs = users.stream()
                    .filter(userTournamentRole -> userTournamentRole.getRole().equals(UserRole.STAFF))
                    .collect(Collectors.toList());
            List<User> staffUsers = new ArrayList<>();
            for(UserTournamentRole u : staffs) staffUsers.add(u.getUser());

            int qtdStaff = staffUsers.size();

            for (Match m : matches){
                m.setUser(staffUsers.get(rand.nextInt(qtdStaff)));
            }
        }

    }





    }


