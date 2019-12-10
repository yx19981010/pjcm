package com.samsph.pjcm.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.dao.JournalRepository;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.query.JournalQuery;
import com.samsph.pjcm.service.JournalService;
import com.samsph.pjcm.vo.*;
import org.dozer.Mapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;
import java.util.Optional;

/**
 * @author hujiahao
 */

@Service
public class JournalServiceImpl implements JournalService {

    @Resource
    private JournalRepository journalRepository;

    @Resource
    private PostRepository postRepository;

    @Resource
    private Mapper dozerMapper;

    private static final String ALREADY_EXISTED = "期刊已存在";
    private static final String NOT_FOUND = "期刊不存在";


    @Override
    public JournalSimpleVO saveJournal(JournalQuery journalQuery,int creatorId) {
        // 查询是否已存在该年、月、卷、期的期刊
        int year = journalQuery.getYear();
        int month = journalQuery.getMonth();
        int volume = journalQuery.getVolume();
        int no = journalQuery.getNumber();
        Optional<Journal> journal = journalRepository.findByYearAndMonthAndVolumeAndNumber(year, month, volume, no);

        // 若存在，则抛出异常
        if (journal.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ALREADY_EXISTED);
        }

        // 若不存在，则保存该期刊信息
        Journal journalPO = dozerMapper.map(journalQuery, Journal.class);

        // 设置创建时间、投稿计数、创建者id
        journalPO.setCreateTime(new Date());
        journalPO.setCount(0);
        journalPO.setCreate_by_uid(creatorId);
        journalPO = journalRepository.save(journalPO);

        return dozerMapper.map(journalPO, JournalSimpleVO.class);
    }

    @Override
    public void deleteJournal(int id) {
        // 查询是否已存在该id的期刊
        Optional<Journal> journal = journalRepository.findById(id);

        // 若不存在则抛出异常
        if (!journal.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, NOT_FOUND);
        }

        // 若存在则删除该记录
        journalRepository.deleteById(id);
    }

    @Override
    public void updateJournal(JournalQuery journalQuery) {
        // 查询是否已存在该id的期刊
        Optional<Journal> journal = journalRepository.findById(journalQuery.getId());

        // 若不存在则抛出异常
        if (!journal.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, NOT_FOUND);
        }

        // 若存在则更新该条记录
        Journal journalPO = journal.get();
        BeanUtil.copyProperties(journalQuery, journalPO, CopyOptions.create().setIgnoreNullValue(true).setIgnoreError(true));
        journalRepository.save(journalPO);
    }

    @Override
    public JournalVO getJournal(int id) {
        // 查询是否已存在该id的期刊
        Optional<Journal> journal = journalRepository.findById(id);

        // 若不存在则抛出异常
        if (!journal.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, NOT_FOUND);
        }

        return encapsulateJournalVO(journal.get());
    }

    @Override
    public JournalVO getJournal(int year, int month, int vol, int no) {
        // 查询是否已存在该年月卷期的期刊
        Optional<Journal> journal = journalRepository.findByYearAndMonthAndVolumeAndNumber(year, month, vol, no);

        // 若不存在则抛出异常
        if (!journal.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, NOT_FOUND);
        }

        return encapsulateJournalVO(journal.get());
    }

    @Override
    public PageData getAll(int page, int size, String sortBy, boolean ascend) {
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        // 请求分页从1开始计数，JPA分页从0计数，故此处进行减1
        PageRequest pageRequest = PageRequest.of(page-1, size, direction, sortBy);
        // 查询分页结果
        Page<Journal> journalPage = journalRepository.findAll(pageRequest);
        int totalPages = journalPage.getTotalPages();
        int totalElements = (int) journalPage.getTotalElements();
        List<Journal> journalLis = journalPage.getContent();
        int cnt = journalLis.size();
        // 封装分页数据并返回
        return new PageData(totalPages, totalElements, page, cnt, DozerUtil.mapList(journalLis, JournalSimpleVO.class));
    }

    /** 查询期刊包含的投稿列表封装成期刊VO
     * @param journal 需要处理的期刊PO
     * @return JournalVO
     */
    private JournalVO encapsulateJournalVO(Journal journal) {
        JournalVO journalVO = dozerMapper.map(journal, JournalVO.class);
        List<Post> postLis = postRepository.findByJid(journalVO.getId());
        journalVO.setPosts(DozerUtil.mapList(postLis, PostSimpleVO.class));

        return journalVO;
    }
}
