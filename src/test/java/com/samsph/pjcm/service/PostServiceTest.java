package com.samsph.pjcm.service;

import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.dao.JournalRepository;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.dao.PostReviewerRepository;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.query.JournalQuery;
import com.samsph.pjcm.query.PostQuery;
import com.samsph.pjcm.query.PostReviewerQuery;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.test.context.junit4.SpringRunner;

import javax.annotation.Resource;

import static com.samsph.pjcm.config.DevUserId.*;
import static org.hamcrest.CoreMatchers.is;

@SpringBootTest
@RunWith(SpringRunner.class)
class PostServiceTest {

    @Resource
    private PostService postService;

    @Resource
    private JournalService journalService;

    @Resource
    private PostReviewerService postReviewerService;

    @Resource
    private PostRepository postRepository;

    @Resource
    private JournalRepository journalRepository;

    @Resource
    private PostReviewerRepository postReviewerRepository;

    private PostQuery saveQuery;

    private Post post;

    @BeforeEach
    void setUp() {
        saveQuery = new PostQuery();
        saveQuery.setField(Field.BASIC_MEDICINE);
        saveQuery.setTitle("论文标题");
        saveQuery.setGenre(Genre.PAPER);
        saveQuery.setFundLevel(FundLevel.MUNICIPAL);
        saveQuery.setWritersInfo("张三;李四");

        post = postService.savePost(saveQuery, CONTRIBUTOR_ID);
    }

    @AfterEach
    void tearDown() {
        postRepository.deleteAll();
        journalRepository.deleteAll();
        postReviewerRepository.deleteAll();
    }

    @Test
    void savePost() {
        Assert.assertThat(post.getField(), is(Field.BASIC_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论文标题"));
        Assert.assertThat(post.getGenre(), is(Genre.PAPER.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.MUNICIPAL.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("张三;李四"));
        Assert.assertThat(post.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));
    }

    @Test
    void getPost() {
        Post post1 = postService.getPost(post.getId());
        Assert.assertThat(post1.getField(), is(Field.BASIC_MEDICINE.getCode()));
        Assert.assertThat(post1.getTitle(), is("论文标题"));
        Assert.assertThat(post1.getGenre(), is(Genre.PAPER.getCode()));
        Assert.assertThat(post1.getFundLevel(), is(FundLevel.MUNICIPAL.getCode()));
        Assert.assertThat(post1.getWritersInfo(), is("张三;李四"));
        Assert.assertThat(post1.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post1.getContributorUid(), is(CONTRIBUTOR_ID));

        try {
            postService.getPost(0);
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("投稿未找到"));
        }
    }

    @Test
    void updatePost() {
        Post post1 = postService.getPost(post.getId());
        post1.setStatus(PostStatus.PENDING_FIRST_EXAM.getCode());
        postService.updatePost(post1);
        post = postService.getPost(post.getId());

        Assert.assertThat(post.getStatus(), is(PostStatus.PENDING_FIRST_EXAM.getCode()));
    }

    @Test
    void getAllByJid() {
        Journal journal = journalService.saveJournal(
                new JournalQuery(null, 2019, 12, 1, 1), ADMIN_ID);

        Page<Post> postPage = postService.getAllByJid(journal.getId(), 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));

        post.setEditorUid(EDITOR_ID);
        postService.updatePost(post);

        postPage = postService.getAllByEd(EDITOR_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));

        post = postPage.getContent().get(0);
        Assert.assertThat(post.getField(), is(Field.BASIC_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论文标题"));
        Assert.assertThat(post.getGenre(), is(Genre.PAPER.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.MUNICIPAL.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("张三;李四"));
        Assert.assertThat(post.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post.getEditorUid(), is(EDITOR_ID));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));
    }

    @Test
    void getAllByCtr() {
        saveQuery.setTitle("论人类的奇怪饲养欲");
        saveQuery.setField(Field.BASIC_CHINESE_MEDICINE);
        saveQuery.setGenre(Genre.OVERVIEW);
        saveQuery.setFundLevel(FundLevel.NO);
        saveQuery.setWritersInfo("阿猫;阿狗");
        postService.savePost(saveQuery, CONTRIBUTOR_ID);
        saveQuery.setTitle("苹果与香蕉");
        saveQuery.setGenre(Genre.WORKS);
        saveQuery.setFundLevel(FundLevel.NATIONAL);
        saveQuery.setWritersInfo("苹果;香蕉");
        postService.savePost(saveQuery, EDITOR_ID);

        Page<Post> postPage = postService.getAllByCtr(CONTRIBUTOR_ID, 1, 3, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(3));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(2L));
        Assert.assertThat(postPage.getNumberOfElements(), is(2));
        Post post = postPage.getContent().get(1);
        Assert.assertThat(post.getField(), is(Field.BASIC_CHINESE_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论人类的奇怪饲养欲"));
        Assert.assertThat(post.getGenre(), is(Genre.OVERVIEW.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.NO.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("阿猫;阿狗"));
        Assert.assertThat(post.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));

        postPage = postService.getAllByCtr(EDITOR_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));
        post = postPage.getContent().get(0);
        Assert.assertThat(post.getField(), is(Field.BASIC_CHINESE_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("苹果与香蕉"));
        Assert.assertThat(post.getGenre(), is(Genre.WORKS.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.NATIONAL.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("苹果;香蕉"));
        Assert.assertThat(post.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post.getContributorUid(), is(EDITOR_ID));

        postPage = postService.getAllByCtr(REVIEWER_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));
    }

    @Test
    void getAllByCtrAndStatus() {
        saveQuery.setTitle("论人类的奇怪饲养欲");
        saveQuery.setField(Field.BASIC_CHINESE_MEDICINE);
        saveQuery.setGenre(Genre.OVERVIEW);
        saveQuery.setFundLevel(FundLevel.NO);
        saveQuery.setWritersInfo("阿猫;阿狗");
        Post post1 = postService.savePost(saveQuery, CONTRIBUTOR_ID);

        Page<Post> postPage = postService.getAllByCtrAndStatus(CONTRIBUTOR_ID, PostStatus.PENDING_FIRST_EXAM, 1, 3, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(3));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));

        post1.setStatus(PostStatus.PENDING_FIRST_EXAM.getCode());
        postService.updatePost(post1);

        postPage = postService.getAllByCtrAndStatus(CONTRIBUTOR_ID, PostStatus.PENDING_FIRST_EXAM, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));
        Post post = postPage.getContent().get(0);
        Assert.assertThat(post.getField(), is(Field.BASIC_CHINESE_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论人类的奇怪饲养欲"));
        Assert.assertThat(post.getGenre(), is(Genre.OVERVIEW.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.NO.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("阿猫;阿狗"));
        Assert.assertThat(post.getStatus(), is(PostStatus.PENDING_FIRST_EXAM.getCode()));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));
    }

    @Test
    void getAllByEd() {
        Page<Post> postPage = postService.getAllByEd(EDITOR_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));

        post.setEditorUid(EDITOR_ID);
        postService.updatePost(post);

        postPage = postService.getAllByEd(EDITOR_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));

        post = postPage.getContent().get(0);
        Assert.assertThat(post.getField(), is(Field.BASIC_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论文标题"));
        Assert.assertThat(post.getGenre(), is(Genre.PAPER.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.MUNICIPAL.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("张三;李四"));
        Assert.assertThat(post.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post.getEditorUid(), is(EDITOR_ID));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));
    }

    @Test
    void getAllByEdAndStatus() {
        Page<Post> postPage = postService.getAllByEdAndStatus(EDITOR_ID, PostStatus.PENDING_FIRST_EXAM, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));

        post.setEditorUid(EDITOR_ID);
        post.setStatus(PostStatus.PENDING_FIRST_EXAM.getCode());
        postService.updatePost(post);

        postPage = postService.getAllByEdAndStatus(EDITOR_ID, PostStatus.PENDING_FIRST_EXAM, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));

        post = postPage.getContent().get(0);
        Assert.assertThat(post.getField(), is(Field.BASIC_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论文标题"));
        Assert.assertThat(post.getGenre(), is(Genre.PAPER.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.MUNICIPAL.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("张三;李四"));
        Assert.assertThat(post.getStatus(), is(PostStatus.PENDING_FIRST_EXAM.getCode()));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));
        Assert.assertThat(post.getEditorUid(), is(EDITOR_ID));
    }

    @Test
    void getAllByRevUnanswer() {
        Page<Post> postPage = postService.getAllByRevUnanswer(REVIEWER_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));

        int id = post.getId();
        postReviewerService.save(new PostReviewerQuery(id, REVIEWER_ID, null));

        postPage = postService.getAllByRevUnanswer(REVIEWER_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));
        post = postPage.getContent().get(0);
        Assert.assertThat(post.getId(), is(id));
    }

    @Test
    void getAllByRev() {
        int id = post.getId();
        PostReviewer postReviewer = postReviewerService.save(new PostReviewerQuery(id, REVIEWER_ID, null));

        Page<Post> postPage = postService.getAllByRev(REVIEWER_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));

        postReviewer.setAccepted(MyBoolean.TRUE.getCode());
        postReviewerService.updatePostReviewer(postReviewer);
        post.setStatus(PostStatus.FIRST_REVIEW.getCode());
        postService.updatePost(post);

        postPage = postService.getAllByRev(REVIEWER_ID, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));
        post = postPage.getContent().get(0);
        Assert.assertThat(post.getId(), is(id));
    }

    @Test
    void getAllRequiredToReview() {
        int id = post.getId();
        PostReviewer postReviewer = postReviewerService.save(new PostReviewerQuery(id, REVIEWER_ID, null));
        postReviewer.setAccepted(MyBoolean.TRUE.getCode());
        postReviewerService.updatePostReviewer(postReviewer);
        post.setStatus(PostStatus.FIRST_REVIEW.getCode());
        postService.updatePost(post);

        Page<Post> postPage = postService.getAllRequiredToReview(REVIEWER_ID, true, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(0));
        Assert.assertThat(postPage.getTotalElements(), is(0L));
        Assert.assertThat(postPage.getNumberOfElements(), is(0));

        postReviewer.setFlag(true);
        postReviewerService.updatePostReviewer(postReviewer);

        postPage = postService.getAllRequiredToReview(REVIEWER_ID, true, 1, 2, true);
        Assert.assertThat(postPage.getNumber(), is(0));
        Assert.assertThat(postPage.getSize(), is(2));
        Assert.assertThat(postPage.getTotalPages(), is(1));
        Assert.assertThat(postPage.getTotalElements(), is(1L));
        Assert.assertThat(postPage.getNumberOfElements(), is(1));
        post = postPage.getContent().get(0);
        Assert.assertThat(post.getId(), is(id));
    }

    @Test
    void deletePost() {
        Post post = postService.savePost(saveQuery, CONTRIBUTOR_ID);

        postService.deletePost(post.getId());

        try {
            postService.getPost(post.getId());
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("投稿未找到"));
        }
    }
}